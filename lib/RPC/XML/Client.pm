###############################################################################
#
# This file copyright (c) 2001 by Randy J. Ray <rjray@blackperl.com>,
# all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License as distributed with Perl versions 5.002 and later. See
# http://language.perl.com/misc/Artistic.html
#
###############################################################################
#
#   $Id: Client.pm,v 1.11 2002/12/05 04:16:15 rjray Exp $
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
#                   request
#
#   Libraries:      LWP::UserAgent
#                   HTTP::Request
#                   URI
#                   RPC::XML
#
#   Global Consts:  $VERSION
#
###############################################################################

package RPC::XML::Client;

use 5.005;
use strict;
use vars qw($VERSION);
use subs qw(new simple_request send_request uri useragent request
            fault_handler error_handler combined_handler);

require LWP::UserAgent;
require HTTP::Request;
require URI;

use RPC::XML 'bytelength';
require RPC::XML::Parser;

$VERSION = do { my @r=(q$Revision: 1.11 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

1;

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
    my $class = shift;
    my $location = shift;
    my %attrs = @_;

    $class = ref($class) || $class;
    return "${class}::new: Missing location argument" unless $location;

    my ($self, $UA, $REQ, $PARSER);

    # Start by getting the LWP::UA object
    $UA = LWP::UserAgent->new() or
        return "${class}::new: Unable to get LWP::UserAgent object";
    $UA->agent(sprintf("%s/%s %s", $class, $VERSION, $UA->agent));
    $self->{__useragent} = $UA;

    # Next get the request object for later use
    $REQ = HTTP::Request->new(POST => $location) or
        return "${class}::new: Unable to get HTTP::Request object";
    $self->{__request} = $REQ;
    $REQ->header(Content_Type => 'text/xml');

    # Check for compression support
    $self->{__compress} = '';
    eval { require Compress::Zlib };
    $self->{__compress} = $@ ? '' : 'deflate';
    # It looks wasteful to keep using the hash key, but it makes it easier
    # to change the string in just one place (above) if I have to.
    $REQ->header(Accept_Encoding  => $self->{__compress})
        if $self->{__compress};
    $self->{__compress_thresh} = $attrs{compress_thresh} || 4096;
    $self->{__compress_re} = qr/$self->{__compress}/;
    # They can change this value with a method
    $self->{__compress_requests} = 0;
    delete $attrs{compress_thresh};

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

    # Preserve any remaining attributes passed in
    $self->{$_} = $attrs{$_} for (keys %attrs);

    # Then, get the RPC::XML::Parser instance
    $PARSER = RPC::XML::Parser->new() or
        return "${class}::new: Unable to get RPC::XML::Parser object";
    $self->{__parser} = $PARSER;

    bless $self, $class;
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

    my ($return, $value);

    $RPC::XML::ERROR = '';

    $return = $self->send_request(@args);
    unless (ref $return)
    {
        $RPC::XML::ERROR = ref($self) . "::simple_request: $return";
        return undef;
    }
    $return->value;
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
#                   $req      in      ref       RPC::XML::request object
#
#   Returns:        Success:    RPC::XML::response object instance
#                   Failure:    error string
#
###############################################################################
sub send_request
{
    my ($self, $req, @args) = @_;

    my ($me, $message, $response, $reqclone, $content, $compress, $value);

    $me = ref($self) . ':send_request';

    if (! UNIVERSAL::isa($req, 'RPC::XML::request'))
    {
        # Assume that $req is the name of the routine to be called
        $req = RPC::XML::request->new($req, @args);
        return "$me: Error creating RPC::XML::request object: $RPC::XML::ERROR"
            unless ($req); # $RPC::XML::ERROR is already set
    }

    $reqclone = $self->{__request}->clone;
    $content = $req->as_string;
    $compress = $self->compress;
    if ($self->compress_requests and $compress and
        length($content) > $self->compress_thresh)
    {
        $content = Compress::Zlib::compress($content);
        $reqclone->content_encoding($compress);
    }
    $reqclone->content($content);
    $reqclone->content_length(bytelength($content));
    $reqclone->header(Host => URI->new($reqclone->uri)->host);
    $response = $self->{__useragent}->request($reqclone);

    unless ($response->is_success)
    {
        $message =  "$me: HTTP server error: " . $response->message;
        return (ref($self->{__error_cb}) eq 'CODE') ?
            $self->{__error_cb}->($message) : $message;
    }

    # Check for compressed content, and decompress it if we can. If we can't,
    # error out.
    if ($compress and
        ($response->content_encoding || '') =~ $self->compress_re)
    {
        $content = Compress::Zlib::uncompress($response->content)
    }
    else
    {
        if (($response->content_encoding || '') =~ /deflate/)
        {
            $message =  "$me: Compressed content encoding not supported";
            return (ref($self->{__error_cb}) eq 'CODE') ?
                $self->{__error_cb}->($message) : $message;
        }
        $content = $response->content;
    }
    # The return value from the parser's parse method no longer works as a
    # direct return value for us
    $value = $self->{__parser}->parse($content);

    # Rather, we now have to check if there is a callback in the case of
    # errors or faults
    if (! ref($value))
    {
        $message =  "$me: parse-level error: $value";
        return (ref($self->{__error_cb}) eq 'CODE') ?
            $self->{__error_cb}->($message) : $message;
    }
    elsif ($value->is_fault)
    {
        return (ref($self->{__fault_cb}) eq 'CODE') ?
            $self->{__fault_cb}->($value->value) : $value->value;
    }

    $value->value;
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
sub uri
{
    my $self = shift;
    $self->{__request}->uri(@_);
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

    $self->{__useragent}->credentials($self->uri(), $realm, $user, $pass);
    $self;
}

# Immutable accessor methods
BEGIN
{
    no strict 'refs';

    for my $method (qw(useragent request compress_re compress))
    {
        *$method = sub { shift->{"__$method"} }
    }
}

# Fetch/set the compression threshhold
sub compress_thresh
{
    my $self = shift;
    my $set = shift || 0;

    my $old = $self->{__compress_thresh};
    $self->{__compress_thresh} = $set if ($set);

    $old;
}

# This doesn't actually *get* the original value, it only sets the value
sub compress_requests
{
    my $self = shift;

    return $self->{__compress_requests} unless @_;

    $self->{__compress_requests} = $_[0] ? 1 : 0;
}

# These are get/set accessors for the fault-handler, error-handler and the
# combined fault/error handler.
sub fault_handler
{
    my ($self, $newval) = @_;

    my $val = $self->{__fault_cb};
    $self->{__fault_cb} = $newval if ($newval and ref($newval));
    # Special: an explicit undef is used to clear the callback
    $self->{__fault_cb} = undef if (@_ == 2 and (! defined $newval));

    $val;
}
sub error_handler
{
    my ($self, $newval) = @_;

    my $val = $self->{__error_cb};
    $self->{__error_cb} = $newval if ($newval and ref($newval));
    # Special: an explicit undef is used to clear the callback
    $self->{__error_cb} = undef if (@_ == 2 and (! defined $newval));

    $val;
}
sub combined_handler
{
    my ($self, $newval) = @_;

    ($self->fault_handler($newval), $self->error_handler($newval));
}

__END__

=pod

=head1 NAME

RPC::XML::Client - An XML-RPC client class

=head1 SYNOPSIS

    require RPC::XML;
    require RPC::XML::Client;

    $cli = RPC::XML::Client->new('http://www.localhost.net/RPCSERV');
    $resp = $cli->send_request('system.listMethods');

    print (ref $resp) ? join(', ', @{$resp->value}) : "Error: $resp";

=head1 DESCRIPTION

This is an XML-RPC client built upon the B<RPC::XML> data classes, and using
B<LWP::UserAgent> and B<HTTP::Request> for the communication layer. This
client supports the full XML-RPC specification.

=head1 METHODS

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

=over 8

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
outgoing messages that are longer than the threshhold setting in bytes.

=item compress_thresh([MIN_LIMIT])

With no arguments, returns the current compression threshhold; messages
smaller than this number of bytes will not be compressed, regardless of the
above method setting. If a number is passed, this is set to the new
lower-limit. The default value is 4096.

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

=head1 CREDITS

The B<XML-RPC> standard is Copyright (c) 1998-2001, UserLand Software, Inc.
See <http://www.xmlrpc.com> for more information about the B<XML-RPC>
specification.

=head1 LICENSE

This module is licensed under the terms of the Artistic License that covers
Perl. See <http://language.perl.com/misc/Artistic.html> for the license
itself.

=head1 SEE ALSO

L<RPC::XML>, L<RPC::XML::Server>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut
