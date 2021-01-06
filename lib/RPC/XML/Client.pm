###############################################################################
#
# This file copyright (c) 2001-2014 Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License 2.0 (http://www.opensource.org/licenses/artistic-license-2.0.php) or
# the GNU LGPL (http://www.opensource.org/licenses/lgpl-2.1.php).
#
###############################################################################
#
#   Description:    This class implements an RPC::XML client, using LWP to
#                   manage the underlying communication protocols. It relies
#                   on the RPC::XML transaction core for data management.
#
#   Functions:      new
#                   send_request
#                   simple_request
#                   uri
#                   useragent
#
#   Libraries:      LWP::UserAgent
#                   HTTP::Request
#                   URI
#                   RPC::XML
#                   RPC::XML::ParserFactory
#                   Compress::Raw::Zlib is used if available
#
#   Global Consts:  $VERSION
#
###############################################################################

package RPC::XML::Client;

use 5.008008;
use strict;
use warnings;
use vars qw($VERSION $COMPRESSION_AVAILABLE);
use subs qw(new simple_request send_request uri useragent request
            fault_handler error_handler combined_handler timeout);

use Scalar::Util 'blessed';
use File::Temp;
use IO::Handle;

use LWP::UserAgent;
use HTTP::Request;
use URI;

use RPC::XML;
use RPC::XML::ParserFactory;

BEGIN
{
    # Check for compression support
    $COMPRESSION_AVAILABLE =
        (eval { require Compress::Zlib; 1; }) ? 'deflate' : q{};
}

$VERSION = '1.44';

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Create a LWP::UA instance and add some extra material
#                   specific to our purposes.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class to bless into
#                   $location in      scalar    URI path for requests to go to
#                   %attrs    in      hash      Extra info
#
#   Globals:        $VERSION
#
#   Returns:        Success:    object reference
#                   Failure:    error string
#
###############################################################################
sub new
{
    my ($class, $location, %attrs) = @_;

    $class = ref($class) || $class;
    if (! $location)
    {
        return "${class}::new: Missing location argument";
    }

    my ($self, $UA, $REQ);

    # Start by getting the LWP::UA object
    $UA = LWP::UserAgent->new(
        (exists $attrs{useragent}) ? @{$attrs{useragent}} : ()
    );
    $UA->agent(sprintf '%s/%s %s', $class, $VERSION, $UA->agent);
    $self->{__useragent} = $UA;
    delete $attrs{useragent};

    # Next get the request object for later use
    $REQ = HTTP::Request->new(POST => $location);
    $self->{__request} = $REQ;
    $REQ->header(Content_Type => 'text/xml');
    $REQ->protocol('HTTP/1.1');

    # Note compression support
    $self->{__compress} = $COMPRESSION_AVAILABLE;
    # It looks wasteful to keep using the hash key, but it makes it easier
    # to change the string in just one place (BEGIN block, above) if I have to.
    # Also (for now) I prefer to manipulate the private keys directly, before
    # blessing $self, rather than using accessors. This is just for performance
    # and I might change my mind later.
    if ($self->{__compress})
    {
        $REQ->header(Accept_Encoding => $self->{__compress});
    }
    $self->{__compress_thresh} = $attrs{compress_thresh} || 4096;
    $self->{__compress_re} = qr/$self->{__compress}/;
    # They can change this value with a method
    $self->{__compress_requests} = 0;
    delete $attrs{compress_thresh};

    # Parameters to control the point at which messages are shunted to temp
    # files due to size, and where to home the temp files. Start with a size
    # threshold of 1Meg and no specific dir (which will fall-through to the
    # tmpdir() method of File::Spec).
    $self->{__message_file_thresh} = $attrs{message_file_thresh} || 1_048_576;
    $self->{__message_temp_dir}    = $attrs{message_temp_dir}    || q{};
    delete @attrs{qw(message_file_thresh message_temp_dir)};

    # Note and preserve any error or fault handlers. Check the combo-handler
    # first, as it is superceded by anything more specific.
    if (ref $attrs{combined_handler})
    {
        $self->{__error_cb} = $attrs{combined_handler};
        $self->{__fault_cb} = $attrs{combined_handler};
        delete $attrs{combined_handler};
    }
    if (ref $attrs{fault_handler})
    {
        $self->{__fault_cb} = $attrs{fault_handler};
        delete $attrs{fault_handler};
    }
    if (ref $attrs{error_handler})
    {
        $self->{__error_cb} = $attrs{error_handler};
        delete $attrs{error_handler};
    }

    # A modification of the pull req from Enrico Sorcinelli. If this attr is
    # non-null then a stringified copy of a request will be saved on the Client
    # object during the duration of a request.
    $self->{__request_as_string} = delete $attrs{request_as_string};

    # Get the RPC::XML::Parser instance from the ParserFactory
    $self->{__parser} =
        RPC::XML::ParserFactory->new($attrs{parser} ? @{$attrs{parser}} : ())
              or return "${class}::new: Unable to get RPC::XML::Parser object";
    delete $attrs{parser};

    # Now preserve any remaining attributes passed in
    for (keys %attrs)
    {
        $self->{$_} = $attrs{$_};
    }

    return bless $self, $class;
}

###############################################################################
#
#   Sub Name:       simple_request
#
#   Description:    Simplify the request process by both allowing for direct
#                   data on the incoming side, and for returning a native
#                   value rather than an object reference.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Class instance
#                   @args     in      list      Various args -- see comments
#
#   Globals:        $RPC::XML::ERROR
#
#   Returns:        Success:    value
#                   Failure:    undef, error in $RPC::XML::ERROR
#
###############################################################################
sub simple_request
{
    my ($self, @args) = @_;

    my $return;

    $RPC::XML::ERROR = q{};

    $return = $self->send_request(@args);
    if (! ref $return)
    {
        $RPC::XML::ERROR = ref($self) . "::simple_request: $return";
        return;
    }

    return $return->value;
}

###############################################################################
#
#   Sub Name:       send_request
#
#   Description:    Take a RPC::XML::request object, dispatch a request, and
#                   parse the response. The return value should be a
#                   RPC::XML::response object, or an error string.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Class instance
#                   $req      in      ref       RPC::XML::request object or
#                                                 remote method name
#                   @args     in      list      If $req is a method name, these
#                                                 are potential arguments for
#                                                 the remote call
#
#   Returns:        Success:    RPC::XML::response object instance
#                   Failure:    error string
#
###############################################################################
sub send_request ## no critic (ProhibitExcessComplexity)
{
    my ($self, $req, @args) = @_;

    my ($me, $message, $response, $reqclone, $content, $can_compress, $value,
        $do_compress, $req_fh, $tmpdir, $com_engine);

    $self->{__request_as_string} && delete $self->{_xmlrpc_request_as_string};

    $me = ref($self) . '::send_request';

    if (! $req)
    {
        return "$me: No request object or remote method name given";
    }
    elsif (! (blessed $req and $req->isa('RPC::XML::request')))
    {
        # Assume that $req is the name of the routine to be called
        if (! ($req = RPC::XML::request->new($req, @args)))
        {
            return "$me: Error creating RPC::XML::request object: " .
                $RPC::XML::ERROR;
        }
    }

    # Add XML-RPC string request as object property if __request_as_string
    # is set.
    if ($self->{__request_as_string}) {
        utf8::encode($self->{_xmlrpc_request_as_string} = $req->as_string);
    }

    # Start by setting up the request-clone for using in this instance
    $reqclone = $self->request->clone;
    if (! $reqclone->header('Host')) {
        $reqclone->header(Host => URI->new($reqclone->uri)->host_port);
    }
    $can_compress = $self->compress; # Avoid making 4+ calls to the method
    if ($self->compress_requests and $can_compress and
        $req->length >= $self->compress_thresh)
    {
        # If this is a candidate for compression, set a flag and note it
        # in the Content-encoding header.
        $do_compress = 1;
        $reqclone->content_encoding($can_compress);
    }

    # Next step, determine our content's disposition. If it is above the
    # threshold for a requested file cut-off, send it to a temp file and use
    # a closure on the request object to manage content.
    if ($self->message_file_thresh and
        $self->message_file_thresh <= $req->length)
    {
        require File::Spec;
        # Start by creating a temp-file
        $tmpdir = $self->message_temp_dir || File::Spec->tmpdir;
        # File::Temp->new() croaks on error, rather than just returning undef
        $req_fh = eval { File::Temp->new(UNLINK => 1, DIR => $tmpdir) };
        if (! $req_fh)
        {
            return "$me: Error opening tmpfile: $@";
        }
        binmode $req_fh;
        # Make it auto-flush
        $req_fh->autoflush();

        # Now that we have it, spool the request to it. This is a little
        # hairy, since we still have to allow for compression. And though the
        # request could theoretically be HUGE, in order to compress we have to
        # write it to a second temp-file first, so that we can compress it
        # into the primary handle.
        if ($do_compress && ($req->length >= $self->compress_thresh))
        {
            my $fh_compress = eval {
                File::Temp->new(UNLINK => 1, DIR => $tmpdir);
            };
            if (! $fh_compress)
            {
                return "$me: Error opening compression tmpfile: $@";
            }
            # Make it auto-flush
            $fh_compress->autoflush();

            # Write the request to the second FH
            $req->serialize($fh_compress);
            seek $fh_compress, 0, 0;

            # Spin up the compression engine
            $com_engine = Compress::Zlib::deflateInit();
            if (! $com_engine)
            {
                return "$me: Unable to initialize the Compress::Zlib engine";
            }

            # Spool from the second FH through the compression engine, into
            # the intended FH.
            my $buf = q{};
            my $out;
            while (read $fh_compress, $buf, 4096)
            {
                $out = $com_engine->deflate(\$buf);
                if (! defined $out)
                {
                    return "$me: Compression failure in deflate()";
                }
                print {$req_fh} $out;
            }
            # Make sure we have all that's left
            $out = $com_engine->flush;
            if (! defined $out)
            {
                return "$me: Compression flush failure in deflate()";
            }
            print {$req_fh} $out;

            # Close the secondary FH. Rewinding the primary is done later.
            if (! close $fh_compress)
            {
                return "$me: Error closing spool-file: $!";
            }
        }
        else
        {
            $req->serialize($req_fh);
        }
        seek $req_fh, 0, 0;

        $reqclone->content_length(-s $req_fh);
        $reqclone->content(sub {
                               my $b = q{};
                               if (! defined read $req_fh, $b, 4096)
                               {
                                   return;
                               }

                               return $b;
                           });
    }
    else
    {
        # Treat the content strictly in-memory
        utf8::encode($content = $req->as_string);
        if ($do_compress)
        {
            $content = Compress::Zlib::compress($content);
        }
        $reqclone->content($content);
        # Because $content has been force-downgraded, length() should work
        $reqclone->content_length(length $content);
    }

    # Content used to be handled as an in-memory string. Now, to avoid eating
    # up huge chunks due to potentially-massive messages (thanks Tellme), we
    # parse incrementally with the XML::Parser::ExpatNB class. What's more,
    # to use the callback-form of request(), we can't get just the headers
    # first. We have to check things like compression and such on the fly.
    my $compression;
    my $parser = $self->parser->parse(); # Gets the ExpatNB object
    my $cb = sub {
        my ($data_in, $resp) = @_;

        if (! defined $compression)
        {
            $compression =
                (($resp->content_encoding || q{}) =~
                 $self->compress_re) ? 1 : 0;
            if ($compression and (! $can_compress))
            {
                die "$me: Compressed content encoding not supported\n";
            }
            if ($compression)
            {
                if (! ($com_engine = Compress::Zlib::inflateInit()))
                {
                    die "$me: Unable to initialize de-compression engine\n";
                }
            }
        }

        if ($compression)
        {
            my $error;
            if (! (($data_in, $error) = $com_engine->inflate($data_in)))
            {
                die "$me: Error in inflate() expanding data: $error\n";
            }
        }

        $parser->parse_more($data_in);
        1;
    };

    $response = $self->useragent->request($reqclone, $cb);
    if ($message = $response->headers->header('X-Died'))
    {
        $parser->release();

        # One of the die's was triggered
        return ('CODE' eq ref $self->error_handler) ?
            $self->error_handler->($message) : $message;
    }
    if (! $response->is_success)
    {
        $parser->release();

        $message =  "$me: HTTP server error: " . $response->message;
        return ('CODE' eq ref $self->error_handler) ?
            $self->error_handler->($message) : $message;
    }

    # Whee. No errors from the callback or the server. Finalize the parsing
    # process.
    if (! eval { $value = $parser->parse_done(); 1; })
    {
        if ($@)
        {
            # One of the die's was triggered
            return ('CODE' eq ref $self->error_handler) ?
                $self->error_handler->($@) : $@;
        }
    }

    # Check if there is a callback to be invoked in the case of
    # errors or faults
    if (! ref $value)
    {
        $message =  "$me: parse-level error: $value";
        return ('CODE' eq ref $self->error_handler) ?
            $self->error_handler->($message) : $message;
    }
    elsif ($value->is_fault)
    {
        return ('CODE' eq ref $self->fault_handler) ?
            $self->fault_handler->($value->value) : $value->value;
    }

    return $value->value;
}

###############################################################################
#
#   Sub Name:       timeout
#
#   Description:    Get or set the timeout() setting on the underlying
#                   LWP::UserAgent object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $time     in      scalar    New timeout value, if passed
#
#   Returns:        Return value from LWP::UserAgent->timeout()
#
###############################################################################
sub timeout ## no critic (RequireArgUnpacking)
{
    my $self = shift;

    return $self->useragent->timeout(@_);
}

###############################################################################
#
#   Sub Name:       uri
#
#   Description:    Get or set the URI portion of the request
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $uri      in      scalar    New URI, if passed
#
#   Returns:        Current URI, undef if trying to set an invalid URI
#
###############################################################################
sub uri ## no critic (RequireArgUnpacking)
{
    my $self = shift;

    return $self->request->uri(@_);
}

###############################################################################
#
#   Sub Name:       credentials
#
#   Description:    Set basic-auth credentials on the underlying user-agent
#                   object
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $realm    in      scalar    Realm to authenticate for
#                   $user     in      scalar    User name to authenticate
#                   $pass     in      scalar    Password for $user
#
#   Returns:        $self
#
###############################################################################
sub credentials
{
    my ($self, $realm, $user, $pass) = @_;

    my $uri = URI->new($self->uri);
    $self->useragent->credentials($uri->host_port, $realm, $user, $pass);

    return $self;
}

# Immutable accessor methods
BEGIN
{
    no strict 'refs'; ## no critic (ProhibitNoStrict)

    for my $method (qw(useragent request compress_re compress parser))
    {
        *{$method} = sub { shift->{"__$method"} }
    }
}

# Fetch/set the compression threshold
sub compress_thresh
{
    my $self = shift;
    my $value = shift || 0;

    my $old = $self->{__compress_thresh};
    if ($value)
    {
        $self->{__compress_thresh} = $value;
    }

    return $old;
}

# This doesn't actually *get* the original value, it only sets the value
sub compress_requests
{
    my ($self, $value) = @_;

    if (! $value)
    {
        return $self->{__compress_requests};
    }

    return $self->{__compress_requests} = $value ? 1 : 0;
}

# These are get/set accessors for the fault-handler, error-handler and the
# combined fault/error handler.
sub fault_handler
{
    my ($self, $newval) = @_;

    my $val = $self->{__fault_cb};
    if ($newval and ref $newval)
    {
        $self->{__fault_cb} = $newval;
    }
    # Special: an explicit undef is used to clear the callback
    if (@_ == 2 and (! defined $newval))
    {
        $self->{__fault_cb} = undef;
    }

    return $val;
}

sub error_handler
{
    my ($self, $newval) = @_;

    my $val = $self->{__error_cb};
    if ($newval and ref $newval)
    {
        $self->{__error_cb} = $newval;
    }
    # Special: an explicit undef is used to clear the callback
    if (@_ == 2 and (! defined $newval))
    {
        $self->{__error_cb} = undef;
    }

    return $val;
}

sub combined_handler
{
    my ($self, $newval) = @_;

    return ($self->fault_handler($newval), $self->error_handler($newval));
}

# Control whether, and at what point, messages are considered too large to
# handle in-memory.
sub message_file_thresh
{
    my ($self, $thresh) = @_;

    if (! $thresh)
    {
        return $self->{__message_file_thresh};
    }

    return $self->{__message_file_thresh} = $thresh;
}

sub message_temp_dir
{
    my ($self, $dir) = @_;

    if (! $dir)
    {
        return $self->{__message_temp_dir};
    }

    return $self->{__message_temp_dir} = $dir;
}

1;

__END__

=pod

=head1 NAME

RPC::XML::Client - An XML-RPC client class

=head1 SYNOPSIS

    require RPC::XML;
    require RPC::XML::Client;

    $cli = RPC::XML::Client->new('http://www.localhost.net/RPCSERV');
    $resp = $cli->send_request('system.listMethods');

    print ref $resp ? join(', ', @{$resp->value}) : "Error: $resp";

=head1 DESCRIPTION

This is an XML-RPC client built upon the B<RPC::XML> data classes, and using
B<LWP::UserAgent> and B<HTTP::Request> for the communication layer. This
client supports the full XML-RPC specification.

=head1 SUBROUTINES/METHODS

The following methods are available:

=over 4

=item new (URI [, ARGS])

Creates a new client object that will route its requests to the URL provided.
The constructor creates a B<HTTP::Request> object and a B<LWP::UserAgent>
object, which are stored on the client object. When requests are made, these
objects are ready to go, with the headers set appropriately. The return value
of this method is a reference to the new object. The C<URI> argument may be a
string or an object from the B<URI> class from CPAN.

Any additional arguments are treated as key-value pairs. Most are attached to
the object itself without change. The following are recognized by C<new> and
treated specially:

=over 4

=item parser

If this parameter is passed, the value following it is expected to be an array
reference. The contents of that array are passed to the B<new> method of the
B<RPC::XML::ParserFactory>-generated object that the client object caches for
its use. See the B<RPC::XML::ParserFactory> manual page for a list of
recognized parameters to the constructor.

=item useragent

This is similar to the C<parser> argument above, and also expects an array
reference to follow it. The contents are passed to the constructor of the
B<LWP::UserAgent> class when creating that component of the client object.
See the manual page for B<LWP::UserAgent> for supported values.

=item error_handler

If passed, the value must be a code reference that will be invoked when a
request results in a transport-level error. The closure will receive a
single argument, the text of the error message from the failed communication
attempt. It is expected to return a single value (assuming it returns at all).

=item fault_handler

If passed, the value must be a code reference. This one is invoked when a
request results in a fault response from the server. The closure will receive
a single argument, a B<RPC::XML::fault> instance that can be used to retrieve
the code and text-string of the fault. It is expected to return a single
value (if it returns at all).

=item combined_handler

If this parameter is specified, it too must have a code reference as a value.
It is installed as the handler for both faults and errors. Should either of
the other parameters be passed in addition to this one, they will take
precedence over this (more-specific wins out over less). As a combined
handler, the closure will get a string (non-reference) in cases of errors, and
an instance of B<RPC::XML::fault> in cases of faults. This allows the
developer to install a simple default handler, while later providing a more
specific one by means of the methods listed below.

=item message_file_thresh

If this key is passed, the value associated with it is assumed to be a
numerical limit to the size of in-memory messages. Any out-bound request that
would be larger than this when stringified is instead written to an anonynous
temporary file, and spooled from there instead. This is useful for cases in
which the request includes B<RPC::XML::base64> objects that are themselves
spooled from file-handles. This test is independent of compression, so even
if compression of a request would drop it below this threshold, it will be
spooled anyway. The file itself is created via File::Temp with UNLINK=>1,
so once it is freed the disk space is immediately freed.

=item message_temp_dir

If a message is to be spooled to a temporary file, this key can define a
specific directory in which to open those files. If this is not given, then
the C<tmpdir> method from the B<File::Spec> package is used, instead.

=item request_as_string

For aiding in debugging, you can pass this key with a non-false value to enable
a step in each request cycle that saves a stringified version of the request
XML as a private key on the client object. The request will be saved to the
key C<_xmlrpc_request_as_string>, and will endure until the next request is
made by the client object.

=back

See the section on the effects of callbacks on return values, below.

=item uri ([URI])

Returns the B<URI> that the invoking object is set to communicate with for
requests. If a string or C<URI> class object is passed as an argument, then
the URI is set to the new value. In either case, the pre-existing value is
returned.

=item useragent

Returns the B<LWP::UserAgent> object instance stored on the client object.
It is not possible to assign a new such object, though direct access to it
should allow for any header modifications or other needed operations.

=item request

Returns the B<HTTP::Request> object. As with the above, it is not allowed to
assign a new object, but access to this value should allow for any needed
operations.

=item simple_request (ARGS)

This is a somewhat friendlier wrapper around the next routine (C<send_request>)
that returns Perl-level data rather than an object reference. The arguments may
be the same as one would pass to the B<RPC::XML::request> constructor, or there
may be a single request object as an argument. The return value will be a
native Perl value. If the return value is C<undef>, an error has occurred and
C<simple_request> has placed the error message in the global variable
C<B<$RPC::XML::ERROR>>.

=item send_request (ARGS)

Sends a request to the server and attempts to parse the returned data. The
argument may be an object of the B<RPC::XML::request> class, or it may be the
arguments to the constructor for the request class. The return value will be
either an error string or a data-type object. If the error encountered was a
run-time error within the RPC request itself, then the call will return a
C<RPC::XML::fault> value rather than an error string.

If the return value from C<send_request> is not a reference, then it can only
mean an error on the client-side (a local problem with the arguments and/or
syntax, or a transport problem). All data-type classes now support a method
called C<is_fault> that may be easily used to determine if the "successful"
return value is actually a C<RPC::XML::fault> without the need to use
C<UNIVERSAL::ISA>.

=item error_handler ([CODEREF])

=item fault_handler ([CODEREF])

=item combined_handler ([CODEREF])

These accessor methods get (and possibly set, if CODEREF is passed) the
specified callback/handler. The return value is always the current handler,
even when setting a new one (allowing for later restoration, if desired).

=item credentials (REALM, USERNAME, PASSWORD)

This sets the username and password for a given authentication realm at the
location associated with the current request URL. Needed if the RPC location
is protected by Basic Authentication. Note that changing the target URL of the
client object to a different (protected) location would require calling this
with new credentials for the new realm (even if the value of C<$realm> is
identical at both locations).

=item timeout ([INTEGER])

Get or set the current time-out value on the underlying B<LWP::UserAgent>
object that this object uses for sending requests. This is just a proxy
through to the method of the same name in the B<LWP::UserAgent> class. The
return value is the current time-out value (prior to change, if a new value
is given).

=item message_file_thresh

=item message_temp_dir

These methods may be used to retrieve or alter the values of the given keys
as defined earlier for the C<new> method.

=back

=head2 Support for Content Compression

The B<RPC::XML::Server> class supports compression of requests and responses
via the B<Compress::Zlib> module available from CPAN. Accordingly, this class
also supports compression. The methods used for communicating compression
support should be compatible with the server and client classes from the
B<XMLRPC::Lite> class that is a part of the B<SOAP::Lite> package (also
available from CPAN).

Compression support is enabled (or not) behind the scenes; if the Perl
installation has B<Compress::Zlib>, then B<RPC::XML::Client> can deal with
compressed responses. However, since outgoing messages are sent before a
client generally has the chance to see if a server supports compression, these
are not compressed by default.

=over 4

=item compress_requests(BOOL)

If a client is communicating with a server that is known to support compressed
messages, this method can be used to tell the client object to compress any
outgoing messages that are longer than the threshold setting in bytes.

=item compress_thresh([MIN_LIMIT])

With no arguments, returns the current compression threshold; messages
smaller than this number of bytes will not be compressed, regardless of the
above method setting. If a number is passed, this is set to the new
lower-limit. The default value is 4096 (4k).

=back

=head2 Callbacks and Return Values

If a callback is installed for errors or faults, it will be called before
either of C<send_request> or C<simple_request> return. If the callback calls
B<die> or otherwise interrupts execution, then there is no need to worry about
the effect on return values. Otherwise, the return value of the callback
becomes the return value of the original method (C<send_request> or
C<simple_request>). Thus, all callbacks are expected, if they return at all,
to return exactly one value. It is recommended that any callback return values
conform to the expected return values. That is, an error callback would
return a string, a fault callback would return the fault object.

=head1 DIAGNOSTICS

All methods return some type of reference on success, or an error string on
failure. Non-reference return values should always be interpreted as errors,
except in the case of C<simple_request>.

=head1 CAVEATS

This began as a reference implementation in which clarity of process and
readability of the code took precedence over general efficiency. It is now
being maintained as production code, but may still have parts that could be
written more efficiently.

=head1 BUGS

Please report any bugs or feature requests to
C<bug-rpc-xml at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=RPC-XML>. I will be
notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=RPC-XML>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/RPC-XML>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/RPC-XML>

=item * Search CPAN

L<http://search.cpan.org/dist/RPC-XML>

=item * MetaCPAN

L<https://metacpan.org/release/RPC-XML>

=item * Source code on GitHub

L<http://github.com/rjray/rpc-xml>

=back

=head1 LICENSE AND COPYRIGHT

This file and the code within are copyright (c) 2011 by Randy J. Ray.

Copying and distribution are permitted under the terms of the Artistic
License 2.0 (L<http://www.opensource.org/licenses/artistic-license-2.0.php>) or
the GNU LGPL 2.1 (L<http://www.opensource.org/licenses/lgpl-2.1.php>).

=head1 CREDITS

The B<XML-RPC> standard is Copyright (c) 1998-2001, UserLand Software, Inc.
See <http://www.xmlrpc.com> for more information about the B<XML-RPC>
specification.

=head1 SEE ALSO

L<RPC::XML|RPC::XML>, L<RPC::XML::Server|RPC::XML::Server>

=head1 AUTHOR

Randy J. Ray C<< <rjray@blackperl.com> >>

=cut
