###############################################################################
#
# This file copyright (c) 2001-2011 Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License 2.0 (http://www.opensource.org/licenses/artistic-license-2.0.php) or
# the GNU LGPL (http://www.opensource.org/licenses/lgpl-2.1.php).
#
###############################################################################
#
#   Description:    This package implements a RPC server as an Apache/mod_perl
#                   content handler. It uses the RPC::XML::Server package to
#                   handle request decoding and response encoding.
#
#   Functions:      handler
#                   init_handler
#                   new
#                   get_server
#                   version
#                   INSTALL_DIR
#                   list_servers
#
#   Libraries:      RPC::XML::Server
#
#   Global Consts:  $VERSION
#
###############################################################################

package Apache::RPC::Server;

use 5.008008;
use strict;
use warnings;
use base qw(RPC::XML::Server);

use Socket;
use File::Spec;

use Apache;
use Apache::File; # For ease-of-use methods like set_last_modified
use Apache::Constants ':common';

use RPC::XML;

## no critic (ProhibitSubroutinePrototypes)

BEGIN
{
    $Apache::RPC::Server::INSTALL_DIR = (File::Spec->splitpath(__FILE__))[1];
    %Apache::RPC::Server::SERVER_TABLE = ();
}

our $VERSION = '1.40';
$VERSION = eval $VERSION; ## no critic (ProhibitStringyEval)

sub version { return $Apache::RPC::Server::VERSION }

sub INSTALL_DIR { return $Apache::RPC::Server::INSTALL_DIR }

# Return a list (not list reference) of currently-known server objects,
# represented as the text-keys from the hash table.
sub list_servers { return keys %Apache::RPC::Server::SERVER_TABLE }

# This is kinda funny, since I don't actually have a debug() method in the
# RPC::XML::Server class at the moment...
sub debug
{
    my ($self, $fmt, @args) = @_;

    my $debug = ref($self) ? $self->SUPER::debug() : 1;

    if ($fmt && $debug)
    {
        Apache::log_error(
            sprintf "%p ($$): $fmt", (ref $self) ? $self : 0, @args
        );
    }

    return $debug;
}

###############################################################################
#
#   Sub Name:       handler
#
#   Description:    This is the default routine that Apache will look for
#                   when we set this class up as a content handler.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Static name of the class we're
#                                                 invoked in
#                   $r        in      ref       Blessed Apache::Request object
#
#   Globals:        $DEF_OBJ
#
#   Returns:        Response code
#
###############################################################################
sub handler ($$) ## no critic (ProhibitExcessComplexity)
{
    my $class = shift;
    my $r = shift;

    my ($srv, $content, $resp, $hdrs, $hdrs_out, $compress, $length,
        $do_compress, $com_engine, $parser, $me, $resp_fh, $c, $peeraddr,
        $peerhost, $peerport);

    $srv = (ref $class) ? $class : $class->get_server($r);
    $me = (ref($class) || $class) . '::handler';
    if (! ref $srv)
    {
        $r->log_error("$me: PANIC! " . $srv);
        return SERVER_ERROR;
    }

    # Set the relevant headers
    $hdrs_out = $r->headers_out;
    $hdrs = $srv->response->headers;
    for (keys %{$hdrs}) { $hdrs_out->{$_} = $hdrs->{$_} }
    $r->content_type('text/xml');
    # We're essentially done if this was a HEAD request
    if ($r->header_only)
    {
        # These headers are either only sent for HEAD requests or are different
        # enough to move here from the above block
        $r->set_last_modified($srv->started);
        $r->send_http_header;
    }
    elsif ($r->method eq 'POST')
    {
        # Step 1: Do we have the correct content-type?
        if ($r->header_in('Content-Type') !~ m{text/xml}i)
        {
            return DECLINED;
        }
        $compress = $srv->compress;
        if ($compress and
            ($r->header_in('Content-Encoding') || q{}) =~ $srv->compress_re)
        {
            $do_compress = 1;
        }

        # Step 2: Read the request in and convert it to a request object
        # Note that this currently binds us to the Content-Length header a lot
        # more tightly than I like. Expect to see this change sometime soon.
        $length = $r->header_in('Content-Length');
        $parser = $srv->parser->parse(); # Get the ExpatNB object
        if ($do_compress)
        {
            # Spin up the compression engine
            if (! ($com_engine = Compress::Zlib::inflateInit()))
            {
                $r->log_error("$me: Unable to init the Compress::Zlib engine");
                return SERVER_ERROR;
            }
        }

        while ($length)
        {
            $r->read($content, ($length < 2048) ? $length : 2048);
            # If $content is undef, then the client has closed the connection
            # on its end, and we're done (like it or not).
            if (! defined $content)
            {
                $r->log_error("$me: Error reading request content");
                return SERVER_ERROR;
            }

            $length -= length $content;
            if ($do_compress)
            {
                if (! ($content = $com_engine->inflate($content)))
                {
                    $r->log_error("$me: Error inflating compressed data");
                    return SERVER_ERROR;
                }
            }
            if (! eval { $parser->parse_more($content); 1; })
            {
                if ($@)
                {
                    $r->log_error("$me: XML parse error: $@");
                    return SERVER_ERROR;
                }
            }
        }

        if (! eval { $content = $parser->parse_done; 1; })
        {
            if ($@)
            {
                $r->log_error("$me: XML parse error at end: $@");
                return SERVER_ERROR;
            }
        }

        # Step 3: Process the request and encode the outgoing response
        # Dispatch will always return a RPC::XML::response object
        {
            # We set some short-lifespan localized keys on $srv to let the
            # methods have access to client connection info
            $c = $r->connection;
            ($peerport, $peeraddr) = unpack_sockaddr_in($c->remote_addr);
            $peerhost = inet_ntoa($peeraddr);
            # Set localized keys on $srv, based on the connection info
            ## no critic (ProhibitLocalVars)
            local $srv->{peeraddr} = $peeraddr;
            local $srv->{peerhost} = $peerhost;
            local $srv->{peerport} = $peerport;
            $resp = $srv->dispatch($content);
        }

        # Step 4: Form up and send the headers and body of the response
        $r->no_cache(1);
        $do_compress = 0; # Clear it
        if ($compress and ($resp->length > $srv->compress_thresh) and
            (($r->header_in('Accept-Encoding') || q{}) =~ $srv->compress_re))
        {
            $do_compress = 1;
            $hdrs_out->{'Content-Encoding'} = $compress;
        }
        # Determine if we need to spool this to a file due to size
        if ($srv->message_file_thresh and
            $srv->message_file_thresh < $resp->length)
        {
            if (! ($resp_fh = Apache::File->tmpfile))
            {
                $r->log_error("$me: Error opening tmpfile");
                return SERVER_ERROR;
            }

            # Now that we have it, spool the response to it. This is a
            # little hairy, since we still have to allow for compression.
            # And though the response could theoretically be HUGE, in
            # order to compress we have to write it to a second temp-file
            # first, so that we can compress it into the primary handle.
            if ($do_compress)
            {
                my $fh_compress = Apache::File->tmpfile;
                if (! $fh_compress)
                {
                    $r->log_error("$me: Error opening second tmpfile");
                    return SERVER_ERROR;
                }

                # Write the request to the second FH
                $resp->serialize($fh_compress);
                seek $fh_compress, 0, 0;

                # Spin up the compression engine
                if (! ($com_engine = Compress::Zlib::deflateInit()))
                {
                    $r->log_error("$me: Unable to initialize the " .
                                  'Compress::Zlib engine');
                    return SERVER_ERROR;
                }

                # Spool from the second FH through the compression engine,
                # into the intended FH.
                my $buf = q{};
                my $out;
                while (read $fh_compress, $buf, 4096)
                {
                    if (! (defined($out = $com_engine->deflate(\$buf))))
                    {
                        $r->log_error("$me: Compression failure in deflate()");
                        return SERVER_ERROR;
                    }
                    print {$resp_fh} $out;
                }
                # Make sure we have all that's left
                if  (! defined($out = $com_engine->flush))
                {
                    $r->log_error("$me: Compression flush failure in deflate");
                    return SERVER_ERROR;
                }
                print {$resp_fh} $out;

                # Close the secondary FH. Rewinding the primary is done
                # later.
                close $fh_compress; ## no critic (RequireCheckedClose)
            }
            else
            {
                $resp->serialize($resp_fh);
            }
            seek $resp_fh, 0, 0;

            $r->set_content_length(-s $resp_fh);
            $r->send_http_header;
            $r->send_fd($resp_fh);
        }
        else
        {
            # Treat the content strictly in-memory
            $content = $resp->as_string;
            if ($do_compress)
            {
                $content = Compress::Zlib::compress($content);
            }
            $r->set_content_length(length $content);
            $r->send_http_header;
            $r->print($content);
        }
    }
    else
    {
        # Flag this as an error, since we don't permit the other methods
        return DECLINED;
    }

    return OK;
}

###############################################################################
#
#   Sub Name:       init_handler
#
#   Description:    Provide a handler for the PerlChildInitHandler phase that
#                   walks through the table of server objects and updates the
#                   child_started time on each.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Calling class (this is a method
#                                                 handler)
#                   $r        in      ref       Apache reference object
#
#   Globals:        %SERVER_TABLE
#
#   Returns:        1
#
###############################################################################
sub init_handler ($$)
{
    my ($class, $r) = @_;

    for (values %Apache::RPC::Server::SERVER_TABLE)
    {
        $_->child_started(1);
    }

    return OK;
}

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Create a new server object, which is blessed into this
#                   class and thus inherits most of the important bits from
#                   RPC::XML::Server.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    String or ref to ID the class
#                   %argz     in      list      Type and relevance of args is
#                                                 variable. See text.
#
#   Globals:        $INSTALL_DIR
#
#   Returns:        Success:    ref to new object
#                   Failure:    error string
#
###############################################################################
sub new ## no critic (ProhibitExcessComplexity)
{
    my ($class, %argz)  = @_;

    my ($R, $servid, $prefix, $self, @dirs, @files, $ret, $no_def,
        $do_auto, $do_mtime);

    $R      = delete $argz{apache} || Apache->server;
    $servid = delete $argz{server_id};
    $prefix = delete $argz{prefix} || q{};
    if (! $argz{path})
    {
        $argz{path} = $R->location;
    }
    if (! $servid)
    {
        $servid = substr $argz{path}, 1;
    }

    # For these Apache-conf type of settings, something explicitly passed in
    # via %argz is allowed to override the config file. So after pulling the
    # value, it is only applied if the corresponding key doesn't already exist

    if (! exists $argz{debug})
    {
        # Is debugging requested?
        $argz{debug} = $R->dir_config("${prefix}RpcDebugLevel") || 0;
    }

    # Check for disabling of auto-loading or mtime-checking
    $do_auto  = $R->dir_config("${prefix}RpcAutoMethods") || 0;
    $do_mtime = $R->dir_config("${prefix}RpcAutoUpdates") || 0;
    foreach ($do_auto, $do_mtime) { $_ = /yes/i ? 1 : 0 }
    if (! exists $argz{auto_methods})
    {
        $argz{auto_methods} = $do_auto;
    }
    if (! exists $argz{auto_updates})
    {
        $argz{auto_updates} = $do_mtime;
    }

    # If there is already an xpl_path, ensure that ours is on the top,
    # otherwise add it.
    if ($argz{xpl_path})
    {
        push @{$argz{xpl_path}}, $Apache::RPC::Server::INSTALL_DIR;
    }
    else
    {
        $argz{xpl_path} = [ $Apache::RPC::Server::INSTALL_DIR ];
    }

    # Create the object, ensuring that the defaults are not yet loaded:
    my $raux = (ref($R) eq 'Apache') ? $R->server : $R;
    $self = $class->SUPER::new(no_default => 1, no_http => 1,
                               path => $argz{path},
                               host => $raux->server_hostname || 'localhost',
                               port => $raux->port,
                               %argz);
    # Non-ref means an error message
    if (! ref $self)
    {
        return $self;
    }
    $self->started('set');

    # Check to see if we should suppress the default methods.
    # The default is "no" (don't suppress the default methods), so use || in
    # the evaluation in case neither were set.
    $no_def = $argz{no_default} ? 1 :
        (($R->dir_config("${prefix}RpcDefMethods") || q{}) =~ /no/i) ? 1 : 0;
    if (! $no_def)
    {
        $self->add_default_methods(-except => 'status.xpl');
        # This should find the Apache version of system.status instead
        $self->add_method('status.xpl');
    }

    # Determine what methods we are configuring for this server instance
    @dirs  = split /:/, ($R->dir_config("${prefix}RpcMethodDir") || q{});
    @files = split /:/, ($R->dir_config("${prefix}RpcMethod")    || q{});
    # Load the directories first, then the individual files. This allows the
    # files to potentially override entries in the directories.
    for (@dirs)
    {
        $ret = $self->add_methods_in_dir($_);
        if (! ref $ret)
        {
            return $ret;
        }
    }
    for (@files)
    {
        $ret = $self->add_method($_);
        if (! ref $ret)
        {
            return $ret;
        }
    }
    if (@dirs)
    {
        # If there were any dirs specified for wholesale inclusion, add them
        # to the search path for later reference.
        $ret = $self->xpl_path;
        unshift @{$ret}, @dirs;
        $self->xpl_path($ret);
    }

    return $Apache::RPC::Server::SERVER_TABLE{$servid} = $self;
}

# Accessor similar to started() that has a time localized to this child process
sub child_started
{
    my ($self, $set_started) = @_;

    my $old = $self->{__child_started} || $self->started || 0;
    if ($set_started)
    {
        $self->{__child_started} = time;
    }

    return $old;
}

###############################################################################
#
#   Sub Name:       get_server
#
#   Description:    Retrieve the server object appropriate for this Server
#                   instance passed in right after $self. If the second arg is
#                   not a reference, assume they are asking for an existing
#                   server by name.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      sc/ref    Object ref or class name
#                   $r        in      ref       Apache interface object ref
#
#   Globals:        %SERVER_TABLE
#
#   Returns:        object ref, either specific or the default object. Sends a
#                   text string if new() fails
#
###############################################################################
sub get_server
{
    my ($self, $r) = @_;

    my ($prefix, $servid, $nocomp);

    if (ref $r)
    {
        # Presume $r to in fact be an Apache reference, and use it as such.
        # If the server that matches this is already in the table, return it.
        # If it isn't, create it from the information we have available.
        $prefix = $r->dir_config('RPCOptPrefix') || q{};
        $servid = $r->dir_config("${prefix}RpcServer") || '<default>';
        $nocomp = $r->dir_config('NoCompression') || q{};


        return $Apache::RPC::Server::SERVER_TABLE{$servid} ||
            $self->new(apache      => $r,
                       server_id   => $servid,
                       prefix      => $prefix,
                       no_compress => $nocomp,
                       path        => $r->location);
    }
    else
    {
        # If $r isn't a reference, then this is likely been called as a class
        # method to get the server object for a specific name. Thus, if it
        # doesn't exist yet, we lack sufficient information to create it on
        # the fly.
        return $Apache::RPC::Server::SERVER_TABLE{$r} ||
            "Error: No such server object '$r' known (yet)";
    }
}

1;

__END__

=pod

=head1 NAME

Apache::RPC::Server - A subclass of RPC::XML::Server tuned for mod_perl

=head1 SYNOPSIS

    # In httpd.conf:
    PerlModule Apache::RPC::Server
    PerlSetVar RpcMethodDir /var/www/rpc:/usr/lib/perl5/RPC-shared
    PerlChildInitHandler Apache::RPC::Server->init_handler
    ...
    <Location /RPC>
        SetHandler perl-script
        PerlHandler Apache::RPC::Server
    </Location>
    </Location /RPC-limited>
        SetHandler perl-script
        PerlHandler Apache::RPC::Server
        PerlSetVar RPCOptPrefix RpcLimit
        PerlSetVar RpcLimitRpcServer Limited
        PerlSetVar RpcLimitRpcMethodDir /usr/lib/perl5/RPC-shared
    </Location>

    # In the start-up Perl file:
    use Apache::RPC::Server;

=head1 DESCRIPTION

The B<Apache::RPC::Server> module is a subclassing of B<RPC::XML::Server> that
is tuned and designed for use within Apache with mod_perl.

Provided are phase-handlers for the general request-processing phase
(C<PerlHandler>) and the child-process initialization phase
(C<PerlChildInitHandler>). The module should be loaded either by inclusion in a
server start-up Perl script or by directives in the server configuration file
(generally F<httpd.con>). One loaded, the configuration file may assign the
module to handle one or more given locations with the general set of
C<E<lt>LocationE<gt>> directives and familiar options. Additional configuration
settings specific to this module are detailed below.

Generally, externally-available methods are provided as files in the XML
dialect explained in L<RPC::XML::Server|RPC::XML::Server>. A subclass derived
from this class may of course use the methods provided by this class and its
parent class for adding and manipulating the method table.

=head1 SUBROUTINES/METHODS

The methods that the server publishes are provided by a combination of the
installation files and Apache configuration values. Details on remote method
syntax and semantics is covered in L<RPC::XML::Server|RPC::XML::Server>.

=head2 Methods

In addition to inheriting all the methods from B<RPC::XML::Server>, the
following methods are either added or overloaded by B<Apache::RPC::Server>:

=over 4

=item handler

This is the default content-handler routine that B<mod_perl> expects when the
module is defined as managing the specified location. This is provided as a
I<method handler>, meaning that the first argument is either an object
reference or a static string with the class name. This allows for other
packages to easily subclass B<Apache::RPC::Server>.

This routine takes care of examining the incoming request, choosing an
appropriate server object to actually process the request, and returning the
results of the remote method call to the client.

=item init_handler

This is another Apache-level handler, this one designed for installation as a
C<PerlChildInitHandler>. At present, its only function is to iterate over all
server object currently in the internal tables and invoke the C<child_started>
method (detailed below) on each. Setting this handler assures that each child
has a correct impression of when it started as opposed to the start time of the
server itself.

Note that this is only applied to those servers known to the master Apache
process. In most cases, this will only be the default server object as
described above. That is because of the delayed-loading nature of all servers
beyond the default, which are likely only in child-specific memory. There are
some configuration options described in the next section that can affect and
alter this.

=item new(HASH)

This is the class constructor. It calls the superclass C<new> method, then
performs some additional steps. These include installing the default methods
(which includes an Apache-specific version of C<system.status>), adding the
installation directory of this module to the method search path, and adding any
directories or explicitly-requested methods to the server object.

The arguments to the constructor are regarded as a hash table (not a hash
reference), and are mostly passed unchanged to the constructor for
B<RPC::XML::Server>. Three parameters are of concern to this class:

=over 8

=item apache

The value associated with this key is a reference to an B<Apache> request
object. If this is not passed, then it is assumed that this is being called in
the start-up phase of the server and the value returned from
C<< Apache->server >> (see L<Apache|Apache>) is used.

=item server_id

This provides the server ID string for the RPC server (not to be confused with
the Apache server) that is being configured.

=item prefix

The prefix is used in retrieving certain configuration settings from the Apache
configuration file.

=back

The server identification string and prefix concepts are explained in more
detail in the next section. See L<RPC::XML::Server|RPC::XML::Server> for a full
list of what additional arguments may be passed to B<new> for eventual proxy to
the parent class constructor.

=item child_started([BOOLEAN])

This method is very similar to the C<started> method provided by
B<RPC::XML::Server>. When called with no argument or an argument that evaluates
to a false value, it returns the UNIX-style time value of when this child
process was started. Due to the child-management model of Apache, this may very
well be different from the value returned by C<started> itself. If given an
argument that evaluates as true, the current system time is set as the new
child-start time.

If the server has not been configured to set this at child initialization, then
the main C<started> value is returned. The name is different so that a child
may specify both server-start and child-start times with clear distinction.

=item get_server(APACHEREQ|STRING)

Get the server object that corresponds to the argument passed. If the argument
is a reference to an B<Apache> request object, use it to determine the name
(by path, etc.) and return that object. If the parameter is not a reference,
it is assumed to be the specific name desired.

If the requested server object does not yet exist, an attempt will be made to
create it and add it to the internal table. The newly-created object is then
returned.

=item list_servers

Return a list of the I<names> used for all the current server instances. Does
not return the server objects themselves (use B<get_server>, above, for that).

=item version

This method behaves exactly like the B<RPC::XML::Server> method, except that
the version string returned is specific to this module instead.

=item INSTALL_DIR

As with B<version>, this is an overload of the parent-class static method that
returns the installation directory of this particular module.

=back

=head2 Apache configuration semantics

In addition to the known directives such as C<PerlHandler> and
C<PerlChildInitHandler>, configuration of this system is controlled through a
variety of settings that are manipulated with the C<PerlSetVar> and
C<PerlAddVar> directives. These variables are:

=over 4

=item RPCOptPrefix [STRING]

Sets a prefix string to be applied to all of the following names before trying
to read their values. Useful for setting within a C<E<lt>LocationE<gt>> block
to ensure that no settings from a higher point in the hierarchy influence the
server being defined.

=item RpcServer [STRING]

Specify the name of the server to use for this location. If not passed, then
the default server is used. This server may also be explicitly requested by the
name "C<C<E<lt>defaultE<gt>>>". If more than one server is going to be created
within the same Apache environment, this setting should always be used outside
the default area so that the default server is not loaded down with extra
method definitions. If a sub-location changes the default server, those changes
will be felt by any location that uses that server.

Different locations may share the same server by specifying the name with this
variable. This is useful for managing varied access schemes, traffic analysis,
etc.

=item RpcMethodDir [DIRECTORY]

This variable specifies directories to be scanned for method C<*.xpl>
files. To specify more than one directory, separate them with "C<:>" just as
with any other directory-path expression. All directories are kept (in the
order specified) as the search path for future loading of methods.

=item RpcMethod [FILENAME]

This is akin to the directory-specification option above, but only provides a
single method at a time. It may also have multiple values separated by
colons. The method is loaded into the server table. If the name is not an
absolute pathname, then it is searched for in the directories that currently
comprise the path. The directories above, however, have not been added to the
search path yet. This is because these directives are processed immediately
after the directory specifications, and thus do not need to be searched. This
directive is designed to allow selective overriding of methods in the
previously-specified directories.

=item RpcDefMethods [YES|NO]

If specified and set to "no" (case-insensitive), suppresses the loading of the
system default methods that are provided with this package. The absence of this
setting is interpreted as a "yes", so explicitly specifying such is not needed.

=item RpcAutoMethods [YES|NO]

If specified and set to "yes", enables the automatic searching for a requested
remote method that is unknown to the server object handling the request. If
set to "no" (or not set at all), then a request for an unknown function causes
the object instance to report an error. If the routine is still not found, the
error is reported. Enabling this is a security risk, and should only be
permitted by a server administrator with fully informed acknowledgement and
consent.

=item RpcAutoUpdates [YES|NO]

If specified and set to "yes", enables the checking of the modification time
of the file from which a method was originally loaded. If the file has
changed, the method is re-loaded before execution is handed off. As with the
auto-loading of methods, this represents a potential security risk, and should
only be permitted by a server administrator with fully informed
acknowledgement and consent.

=back

=head2 Specifying methods to the server(s)

Methods are provided to an B<Apache::RPC::Server> object in three ways:

=over 4

=item Default methods

Unless suppressed by a C<RpcDefMethods> option, the methods shipped with this
package are loaded into the table. The B<Apache::RPC::Server> objects get a
slightly different version of C<system.status> than the parent class does.

=item Configured directories

All method files (those ending in a suffix of C<*.xpl>) in the directories
specified in the relevant C<RpcMethodDir> settings are read next. These
directories are also (after the next step) added to the search path the object
uses.

=item By specific inclusion

Any methods specified directly by use of C<RpcMethod> settings are loaded
last. This allows for them to override methods that may have been loaded from
the system defaults or the specified directories.

=back

If a request is made for an unknown method, the object will first attempt to
find it by searching the path of directories that were given in the
configuration as well as those that are part of the system (installation-level
directories). If it is still not found, then an error is reported back to the
requestor. By using this technique, it is possible to add methods to a running
server without restarting it. It is a potential security hole, however, and it
is for that reason that the previously-documented C<RpcAutoMethods> setting is
provided.

=head2 Usage Within <Perl> Sections

To truly unlock the power of having the RPC server attached to a B<mod_perl>
environment, the application and configuration of the server should be done
within Perl-configuration blocks on the Apache server itself.

In doing this, two immediate benefits are gained:

=over 4

=item (1)

The rpc-server object gets created in the master Apache process, rather than
within each child as a side-effect of the first request.  Especially in cases
where there are going to be more than one server in use within the Apache
environment, this boosts performance by allowing newly-created children to
already have the server object and method table readily available.

=item (2)

It becomes possible to exert more detailed control over the creation and
configuration of each server object. Combining the B<get_method> and
B<add_method> operations permits "sharing" (of a sort) of methods between
server objects. Recall from the B<RPC::XML::Server> documentation that, when a
method is invoked, the first argument is the server object that is marshalling
it.

=back

The following example illustrates these concepts in a fairly simple
environment:

    # In httpd.conf:
    <Perl>

    # First, create and configure some Apache::RPC::Server objects

    # One regular one, with the standard settings:
    $main::defobj = Apache::RPC::Server->new(path         => '/RPC',
                                             auto_methods => 1,
                                             auto_updates => 1);
    # One version without the default methods, and no auto-actions
    $main::secobj = Apache::RPC::Server->new(no_default => 1,
                                             path => '/rpc-secured');

    # Imagine that add_method and/or add_methods_in_dir has been used to
    # add to the methods tables for those objects. Now assign them to
    # locations managed by Apache:
    $Location{'/RPC'} =
        {
            SetHandler  => 'perl-script',
            PerlHandler => '$main::defobj'
        };
    $Location{'/rpc-secure'} =
        {
            SetHandler   => 'perl-script',
            PerlHandler  => '$main::secobj',
            AuthUserFile => '/etc/some_file',
            AuthType     => 'Basic',
            AuthName     => 'SecuredRPC',
            'require'    => 'valid-user'
        };

    </Perl>

Note that the assignment of the C<PerlHandler> value was a string
representation of the object reference itself. B<mod_perl> performs a sort of
"thaw" of this string when the location is accessed. Since this class
implements itself as a I<method handler>, this causes the C<handler()> method
for each of the locations to be handed the B<Apache::RPC::Server> object
directly. Note also that the value assigned to C<PerlHandler> cannot be a
lexical variable, or it will be out of scope when the handler is called.

=head1 DIAGNOSTICS

All methods return some type of reference on success, or an error string on
failure. Non-reference return values should always be interpreted as errors
unless otherwise noted.

Where appropriate, the C<log_error> method from the B<Apache> package
is called to note internal errors.

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

L<RPC::XML::Server|RPC::XML::Server>, L<RPC::XML|RPC::XML>

=head1 AUTHOR

Randy J. Ray C<< <rjray@blackperl.com> >>

=cut
