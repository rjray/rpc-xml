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
#   $Id: Client.pm,v 1.2 2001/05/08 08:44:51 rjray Exp $
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
use subs qw(new send_request uri useragent request);

require LWP::UserAgent;
require HTTP::Request;
require URI;

require RPC::XML;
require RPC::XML::Parser;

$VERSION = do { my @r=(q$Revision: 1.2 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

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
#   Environment:    None.
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
    $REQ->header(Content_Type   => 'text/xml');

    # Preserve any attributes passed in
    $self->{lc $_} = $attrs{$_} for (keys %attrs);

    # Then, get the RPC::XML::Parser instance (so that we have washed attrs)
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
#   Environment:    None.
#
#   Returns:        Success:    value
#                   Failure:    void return, error in $RPC::XML::ERROR
#
###############################################################################
sub simple_request
{
    my $self = shift;
    my @args = @_;

    my ($return, $value);

    $RPC::XML::ERROR = '';
    unless (@args == 1 and UNIVERSAL::isa($args[0], 'RPC::XML::request'))
    {
        # Assume that this is either data for a new request object or a set
        # of objects meant to be a composite request.
        $value = RPC::XML::request->new(@args);
        return unless ($value); # $RPC::XML::ERROR is already set
        $args[0] = $value;
    }

    $return = $self->send_request($args[0]);
    unless (ref $return)
    {
        $RPC::XML::ERROR = ref($self) . "::simple_request: $return";
        return;
    }
    $return->value->value;
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
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        Success:    RPC::XML::response object instance
#                   Failure:    error string
#
###############################################################################
sub send_request
{
    my $self = shift;
    my $req = shift;

    return ref($self) . '::request: Parameter in must be a RPC::XML::request'
        unless (UNIVERSAL::isa($req, 'RPC::XML::request'));

    my ($respxml, $response, $reqclone);

    ($reqclone = $self->{__request}->clone)->content($req->as_string);
    $reqclone->header(Host => URI->new($reqclone->uri)->host);
    $response = $self->{__useragent}->request($reqclone);

    return ref($self) . '::request: HTTP server error: ' . $response->message
        unless ($response->is_success);
    $respxml = $response->content;

    # The return value from the parser's parse method works for us
    $self->{__parser}->parse($respxml);
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
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        Current URI, undef if trying to set an invalid URI
#
###############################################################################
sub uri
{
    my $self = shift;
    my $uri  = shift;

    $self->{__request}->uri($uri);
}

# Accessor methods for the LWP::UserAgent and HTTP::Request objects
sub useragent { shift->{__useragent} }
sub request   { shift->{__request}   }

__END__

=pod

=head1 NAME

RPC::XML::Client - Sample implementation of a RPC::XML client

=head1 SYNOPSIS

    require RPC::XML;
    require RPC::XML::Client;

    $cli = new RPC::XML::Client 'http://www.localhost.net/RPCSERV';
    $resp = $cli->send_request(RPC::XML::request->new('system.listMethods');

    # Assuming a successful return, should produce a well-formed XML doc
    print $resp->as_string;

=head1 DESCRIPTION

This is a sample XML-RPC client built upon the B<RPC::XML> data classes, and
using B<LWP::UserAgent> and B<HTTP::Request> for the communication layer. This
client supports the full XML-RPC specification.

=head1 METHODS

The following methods are available:

=over

=item new (URI)

Creates a new client object that will route its requests to the URL provided.
The constructor creates a B<HTTP::Request> object and a B<LWP::UserAgent>
object, which are stored on the client object. When requests are made, these
objects are ready to go, with the headers set appropriately. The return value
of this method is a reference to the new object. The C<URI> argument may be a
string or an object from the B<URI> class from CPAN.

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

This is a somewhat friendlier wrapper around the next routine
(C<send_request>) that allows for more flexibility on the input side, and
returns Perl-level data rather than an object reference. The arguments may be
the same as one would pass to the B<RPC::XML::request> constructor, or there
may be a single request object as an argument. The return value will be a
native Perl value. If the return value is C<undef>, this could be due to
either an actual return value from the request, or an error. C<simple_request>
clears the global error variable B<C<$RPC::XML::ERROR>> before the call, and
as such the developer may assume that if this variable has data upon return,
then the empty return value is due to an error.

=item send_request (REQ)

Sends a request to the server and attempts to parse the returned data. The
argument is an object of the B<RPC::XML::request> class, and the return value
will be either an error string or a response object. See L<RPC::XML> for
more on the response class and its methods. If the error encountered was a
run-time error within the RPC request itself, then the client will return a
response object that encapsulates a C<RPC::XML::fault> value rather than an
error string.

=back

=head1 DIAGNOSTICS

All methods return some type of reference on success, or an error string on
failure. Non-reference return values should always be interpreted as errors.

=head1 CAVEATS

This is a reference implementation in which clarity of process and readability
of the code took precedence over general efficiency. Much, if not all, of this
can be written more compactly and/or efficiently.

=head1 CREDITS

The B<XML-RPC> standard is Copyright (c) 1998-2001, UserLand Software, Inc.
See <http://www.xmlrpc.com> for more information about the B<XML-RPC>
specification.

=head1 SEE ALSO

L<RPC::XML>, L<RPC::XML::Server>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut
