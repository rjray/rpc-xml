###############################################################################
#
# This file copyright (c) 2001 by Randy J. Ray <rjray@blackperl.com>,
# all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License as distributed with Perl versions 5.005 and later. See
# http://language.perl.com/misc/Artistic.html
#
###############################################################################
#
#   $Id: Server.pm,v 1.3 2001/06/07 09:26:49 rjray Exp $
#
#   Description:    This package implements a RPC server as an Apache/mod_perl
#                   content handler. It uses the RPC::XML::Server package to
#                   handle request decoding and response encoding.
#
#   Functions:      handler
#                   new
#
#   Libraries:      RPC::XML::Server
#
#   Global Consts:  $VERSION
#
###############################################################################

package Apache::RPC::Server;

use 5.005;
use strict;

use Apache;
use Apache::File; # For ease-of-use methods like set_last_modified
use Apache::Constants ':common';

use RPC::XML::Server;
@Apache::RPC::Server::ISA = qw(RPC::XML::Server);

BEGIN
{
    ($Apache::RPC::Server::INSTALL_DIR) = (__FILE__ =~ m|(.*)/|);
    %Apache::RPC::Server::SERVER_TABLE = ();
}

$Apache::RPC::Server::VERSION = do { my @r=(q$Revision: 1.3 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

1;

sub version { $Apache::RPC::Server::VERSION }

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
#   Environment:    None.
#
#   Returns:        Response code
#
###############################################################################
sub handler ($$)
{
    my $class = shift;
    my $r = shift;

    my ($self, $srv, $content, $resp, $respxml);

    $srv = (ref $class) ? $class : $class->get_server($r);
    unless (ref $srv)
    {
        $r->log_error(__PACKAGE__ . ': PANIC! ' . $srv);
        return SERVER_ERROR;
    }

    # Set the relevant headers
    my $hdrs = $srv->response->headers;
    for (keys %$hdrs) { $r->header_out($_ => $hdrs->{$_}) }
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
        return DECLINED unless ($r->header_in('Content-Type') eq 'text/xml');
        $r->read($content, $r->header_in('Content-Length'));

        # Step 2: Process the request and encode the outgoing response
        # Dispatch will always return a RPC::XML::response
        $resp = $srv->dispatch(\$content);
        $respxml = $resp->as_string;

        # Step 3: Form up and send the headers and body of the response
        $r->content_type('text/xml');
        $r->set_content_length(length $respxml);
        $r->no_cache(1);
        $r->send_http_header;
        $r->print(\$respxml);
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
#   Environment:    None.
#
#   Returns:        1
#
###############################################################################
sub init_handler ($$)
{
    my ($class, $r) = @_;

    $_->child_started(1) for (values %Apache::RPC::Server::SERVER_TABLE);

    1;
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
#                   @argz     in      list      Type and relevance of args is
#                                                 variable. See text.
#
#   Globals:        $INSTALL_DIR
#
#   Environment:    None.
#
#   Returns:        Success:    ref to new object
#                   Failure:    error string
#
###############################################################################
sub new
{
    my $class = shift;
    my @argz  = @_;

    my ($R, $servid, $prefix, $self, @dirs, @files, $package, $ret);

    if ($argz[0] eq 'set-default')
    {
        # Assume that this leading argument is only called from the BEGIN block
        # and just clobber @argz. Serves anyone right for trying to out-tricky
        # me
        $R = Apache->server;
        $servid = '<default>';
        $prefix = '';
        @argz = (path => '/', no_http => 1,
                 xpl_path => [ $Apache::RPC::Server::INSTALL_DIR ]);
    }
    else
    {
        ($R, $servid, $prefix) = splice(@argz, 0, 3);
    }

    # Create the object, ensuring that the defaults are not yet loaded:
    $self = $class->SUPER::new(no_default => 1, @argz);
    return $self unless (ref $self); # Non-ref means an error message
    $self->started('set');
    $self->add_default_methods(-except => 'status.xpl');
    $self->add_method('status.xpl');  # This should find the Apache one instead

    # Determine what methods we are configuring for this server instance
    @dirs    = $R->dir_config->get("${prefix}RpcMethodDir");
    @files   = $R->dir_config->get("${prefix}RpcMethod");
    $package = $R->dir_config("${prefix}RpcMethodPackage");
    # Load the directories first, then the individual files. This allows the
    # files to potentially override entries in the directories.
    for (@dirs)
    {
        $ret = $self->add_methods_in_dir($_);
        return $ret unless ref $ret;
    }
    for (@files)
    {
        $ret = $self->add_method($_);
        return $ret unless ref $ret;
    }

    $self;
}

# Accessor similar to started() that has a time localized to this child process
sub child_started
{
    my $self = shift;
    my $set  = shift || 0;

    my $old = $self->{__child_started} || $self->{__started} || 0;
    $self->{__child_started} = time if $set;

    $old;
}

###############################################################################
#
#   Sub Name:       get_server
#
#   Description:    Retrieve the server object for the specified fully-qual'd
#                   URL passed in as arg #2. Note that this isn't a class
#                   method-- it's only called by handler() and the first arg
#                   is the Apache object reference.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      sc/ref    Object ref or class name
#                   $r        in      ref       Apache interface object ref
#
#   Globals:        %SERVER_TABLE
#
#   Environment:    None.
#
#   Returns:        object ref, either specific or the default object. Sends a
#                   text string if new() fails
#
###############################################################################
sub get_server
{
    my $self     = shift;
    my $r        = shift;

    my $prefix = $r->dir_config('RPCOptPrefix') || '';
    my $servid = $r->dir_config("${prefix}RpcServer") || '<default>';

    $Apache::RPC::Server::SERVER_TABLE{$servid} ||
        $self->new($r, $servid, $prefix,
                   # These are parameters that bubble up to the SUPER::new()
                   xpl_path => [ $Apache::RPC::Server::INSTALL_DIR ],
                   no_http  => 1, # We, um, have that covered
                   path     => $r->uri);
}

__END__
