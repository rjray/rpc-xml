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
#   Description:    This module is intended to provide a browser-friendly
#                   status page on the RPC server(s) being managed by the
#                   hosting Apache process.
#
#                   Some parts of this are borrowed from the Apache::Status
#                   module.
#
#   Functions:      new
#                   version
#                   handler
#                   init_handler
#                   apache_status_attach
#                   header
#                   footer
#                   make_url
#                   main_screen
#                   server_summary
#                   server_detail
#                   method_summary
#                   method_detail
#
#   Libraries:      Apache
#                   Apache::Constants
#
#   Global Consts:  $Apache::RPC::Status::VERSION
#
#   Environment:    None.
#
###############################################################################

package Apache::RPC::Status;

use 5.008008;
use strict;
use warnings;
use vars qw(%IS_INSTALLED $SERVER_VER $STARTED $PERL_VER $DEFAULT
            $SERVER_CLASS);
use subs qw(header footer main_screen server_summary server_detail
            method_summary method_detail);

use Apache;
use Apache::Constants qw(DECLINED OK SERVER_VERSION);
use CGI;

## no critic (ProhibitSubroutinePrototypes)

# We use the server module to get the class methods for server objects, etc.
require Apache::RPC::Server;
require RPC::XML::Procedure;

$SERVER_CLASS = 'Apache::RPC::Server';
$STARTED    = scalar localtime $^T;
$PERL_VER   = $^V ? sprintf 'v%vd', $^V : $];

our $VERSION = '1.13';
$VERSION = eval $VERSION; ## no critic (ProhibitStringyEval)

#
# %proto is the prototype set of screens/handlers that this class knows about.
# It is used in new() to initialize the hash table.
#
my %proto = ( main   => { title => 'Main Screen', call => \&main_screen },
              server => { title => 'Server Detail Screen',
                          call => \&server_detail },
              method => { title => 'Method Detail Screen',
                          call => \&method_detail }, );

# This is an artifact, but things don't seem to work without it
my $newq = sub { CGI->new; };

#
# This next bit graciously "borrowed" from Apache::Status
#
my %IS_INSTALLED = ();
{
    local $SIG{__DIE__}; ## no critic (RequireInitializationForLocalVars)
    %IS_INSTALLED = map {
        ($_, (eval("require $_") || 0)); ## no critic (ProhibitStringyEval)
    } qw(Data::Dumper Devel::Symdump B Apache::Request Apache::Peek
         Apache::Symbol);
}

# Simple token-response method
sub version { return $Apache::RPC::Status::VERSION }

sub new
{
    my ($class, @args) = @_;

    my %self = %proto;

    return bless \%self, $class;
}

# This retrieves the default object for use within handler() below. Basically,
# handler() needs a blessed reference to operate on so that it can call the
# header() and footer() routines as methods to allow for subclassing.
sub default_object
{
    my ($class, @args) = @_;

    return $DEFAULT if (ref $DEFAULT);

    return $DEFAULT = $class->new(@args);
}

###############################################################################
#
#   Sub Name:       handler
#
#   Description:    This is the basic entry point for the majority of uses
#                   for this module. It handles requests at the usual content
#                   phase of the request cycle.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      scalar    Either a class name (if static)
#                                                 or a reference
#                   $r        in      Apache    The request object
#
#   Returns:        Apache code (either OK or DECLINED)
#
###############################################################################
sub handler ($$)
{
    my $self = shift;
    my $r    = shift;

    my ($qs, $pick);

    if (! ref $self)
    {
        $self = $self->default_object();
    }
    $qs = $newq->($r);
    $pick = $qs->param('screen') || 'main';
    # One last check
    if (! exists $self->{$pick})
    {
        return DECLINED
    }

    $self->header($r, $self->{$pick}{title});
    $r->print(@{$self->{$pick}{call}->($self, $r, $qs)});
    $self->footer($r);

    return OK;
}

###############################################################################
#
#   Sub Name:       init_handler
#
#   Description:    Perform any child-proc-specific initialization. Must be
#                   set as a PerlChildInitHandler.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class or handler reference
#                   $r        in      Apache    Request object
#
#   Globals:        $SERVER_CLASS
#
#   Returns:        Apache code (currently always OK)
#
###############################################################################
sub init_handler ($$)
{
    my ($class, $r) = @_;

    if (my $val = $r->dir_config('ServerClass'))
    {
        $SERVER_CLASS = $val;
    }

    return OK;
}

###############################################################################
#
#   Sub Name:       apache_status_attach
#
#   Description:    Attach to the Apache::Status mechanism, if possible. The
#                   object that calls this method will be used to dispatch
#                   any future requests. That means that there is a dangling
#                   reference to it in the closure that is created here, and
#                   which likely lives somewhere within Apache::Status. Just
#                   in case you some day wonder why your object appears to
#                   linger...
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object reference
#
#   Returns:        void
#
###############################################################################
sub apache_status_attach
{
    my $self = shift;

    my $class = ref($self) || $self;

    if (Apache->module('Apache::Status'))
    {
        Apache::Status->
              menu_item(XMLRPC => "$class Monitor",
                        sub {
                            my ($r, $q) = @_; #request and CGI objects
                            my $hook = $q->param('screen') || 'main';

                            $self->{$hook}{call}->($self, $r, $q, 1);
                        });
    }

    return;
}

###############################################################################
#
#   Sub Name:       header
#
#   Description:    Produce the HTML header to start a generic page
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Class object
#                   $r        in      ref       Apache request object
#                   $title    in      scalar    If passed, extra text for the
#                                                 title
#
#   Globals:        $SERVER_VER
#                   $STARTED
#                   $PERL_VER
#
#   Returns:        void
#
###############################################################################
sub header
{
    my ($self, $r, $title) = @_;

    if (! $SERVER_VER)
    {
        $SERVER_VER = SERVER_VERSION;
    }

    if ($title)
    {
        $title = " - $title";
    }
    $title = ref($self) . $title;

    $r->send_http_header('text/html');
    $r->print(<<"EOF");
<html>
<head><title>$title</title></head>
<body bgcolor="white">
<p>Perl version <b>$PERL_VER</b> for <b>$SERVER_VER</b> process <b>$$</b>,<br>
running since $STARTED</p>
<hr>
EOF

    return;
}

###############################################################################
#
#   Sub Name:       footer
#
#   Description:    Close out the current HTML page
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Class object
#                   $r        in      ref       Apache request object
#
#   Returns:        void
#
###############################################################################
sub footer
{
    my ($self, $r) = @_;

    my $name = ref $self;
    my $vers = $self->version;
    my $date = scalar localtime;

    $r->print(<<"EOF");
<hr>
<table width="100%">
<tr>
<td><i>$name $vers</i></td>
<td align="right">$date</td>
</tr>
</table>
</body>
</html>
EOF

    return;
}

###############################################################################
#
#   Sub Name:       make_url
#
#   Description:    Simple url-generation routine that preserves params from
#                   the CGI (or Apache) object, and pays attention to whether
#                   the URL should be patterned for use under Apache::Status
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class, ignored
#                   $query    in      ref       Query or Apache object ref
#                   $flag     in      scalar    If passed and true, create a
#                                                 URI for Apache::Status
#
#   Returns:        string
#
###############################################################################
sub make_url
{
    my ($class, $query, $flag) = @_;

    if (ref $query ne 'CGI')
    {
        $query = $newq->($query);
    }

    my @params = map {
        ($_ eq 'keywords') ? () : "$_=" . $query->param($_)
    } ($query->param());
    my $text = $query->url(-path => 1) . q{?};

    if ($flag)
    {
        unshift @params, 'RPCXML';
    }
    $text .= join q{&} => @params;

    return $text;
}

###############################################################################
#
#   Sub Name:       main_screen
#
#   Description:    Produce the HTML body for the main status screen.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $R        in      ref       Apache object reference
#                   $Q        in      CGI       Query object
#                   $flag     in      scalar    If passed and true, this means
#                                                 that the call is coming from
#                                                 within Apache::Status
#
#   Globals:        $SERVER_CLASS
#
###############################################################################
sub main_screen
{
    my ($self, $R, $Q, $flag) = @_;

    my (@servers, $server, $uri, @lines);

    # Set (or override) the param value for 'screen' before calling make_url
    $Q->param(-name => 'screen', -value => 'server');
    $uri = $self->make_url($Q, $flag);
    @servers = sort $SERVER_CLASS->list_servers();

    push @lines, $Q->p($Q->b('Apache XML-RPC Status Monitor'));
    push @lines,
         sprintf '<p>There %s %d server%s configured:</p>',
         (@servers == 1) ?
         ('is', 1, q{}) : ('are', scalar(@servers), q{s});
    push @lines,
         $Q->table({ -cellpadding => 15, -width => '75%', -border => 0 },
                   (map { ## no critic (ProhibitComplexMappings)
                       ($server = $_) =~ s/</&lt;/g;

                       $Q->TR({ -valign => 'top' },
                              $Q->td({ -width => '35%' },
                                     # I'm adding server=n here to avoid extra
                                     # calls to make_url()
                                     $Q->a({ -href => "$uri&server=$_" },
                                           $server)),
                              $Q->td(server_summary($Q,
                                                    $SERVER_CLASS->
                                                    get_server($_))));
                   } (@servers)));

    return \@lines;
}

###############################################################################
#
#   Sub Name:       server_summary
#
#   Description:    Produce the summary table of server info for the main
#                   status page.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $Q        in      CGI       Query object (for HTML bits)
#                   $srv      in      ref       Server object reference
#
#   Returns:        text chunk
#
###############################################################################
sub server_summary
{
    my ($Q, $srv) = @_;

    return $Q->table($Q->TR({ -valign => 'top' },
                            $Q->td($Q->b($Q->tt('URI:'))),
                            $Q->td($srv->url())),
                     $Q->TR({ -valign => 'top' },
                            $Q->td($Q->b($Q->tt('Requests:'))),
                            $Q->td($srv->requests())),
                     $Q->TR({ -valign => 'top' },
                            $Q->td($Q->b($Q->tt('Started:'))),
                            $Q->td(scalar localtime $srv->started())),
                     $Q->TR({ -valign => 'top' },
                            $Q->td($Q->b($Q->tt('Available methods:'))),
                            $Q->td(scalar($srv->list_methods))));
}

###############################################################################
#
#   Sub Name:       server_detail
#
#   Description:    Provide a detailed break-down screen for a single
#                   server object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $R        in      ref       Apache object reference
#                   $Q        in      CGI       Query object
#                   $flag     in      scalar    If passed and true, means that
#                                                 we are called from with the
#                                                 Apache::Status module
#
#   Globals:        $SERVER_CLASS
#
###############################################################################
sub server_detail
{
    my ($self, $R, $Q, $flag) = @_;

    my ($srv, $server, @lines, @methods, $meth_left, $meth_right, $base_url);

    $server = $Q->param('server');
    # Override this before calling make_url:
    $Q->param(-name => 'screen', -value => 'method');
    # Now create the base URL string for method_summary to use
    $base_url = $self->make_url($Q, $flag);
    if (! $server)
    {
        return [ 'Error: No server name specified when screen invoked' ];
    }
    elsif (! ref($srv = $SERVER_CLASS->get_server($server)))
    {
        return [ "Error fetching server named $server: $srv" ];
    }

    push @lines, '<div align="center">', $Q->b('Server: '), $Q->tt($server);
    push @lines, $Q->br(), $Q->br();
    push @lines, '<table border="0" width="75%">';
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('Server Tokens:')),
                        $Q->td($Q->tt($srv->product_tokens)));
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('Server URL:')),
                        $Q->td($Q->tt($srv->url)));
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('Server Started:')),
                        $Q->td($Q->tt(scalar localtime $srv->started())));
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('This Child Started:')),
                        $Q->td($Q->tt(scalar localtime $srv->child_started)));
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('Requests Handled:')),
                        $Q->td($Q->tt($srv->requests)));
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('Method Search Path:')),
                        $Q->td($Q->tt(join $Q->br() => @{$srv->xpl_path})));
    push @lines, $Q->TR($Q->td({ colspan => 2 }, '&nbsp;'));
    @methods = sort $srv->list_methods;
    if (@methods)
    {
        push @lines, $Q->TR($Q->td({ colspan => 2, -align => 'center' },
                                   $Q->b('Known Methods: '),
                                   sprintf '(%d)', scalar @methods));
        push @lines, '<tr><td colspan="2"><table width="100%" border="1">';
        while (@methods)
        {
            ($meth_left, $meth_right) = splice @methods, 0, 2;
            push @lines, '<tr valign="top"><td width="50%">';
            push @lines, method_summary($Q, $server,
                                        $srv->get_method($meth_left),
                                        $base_url);
            push @lines, '</td><td width="50%">';
            if ($meth_right)
            {
                push @lines, method_summary($Q, $server,
                                            $srv->get_method($meth_right),
                                            $base_url);
            }
            else
            {
                push @lines, '&nbsp;';
            }
            push @lines, '</td></tr>';
        }
        push @lines, '</table></td></tr>';
    }
    push @lines, '</table></div>';

    return \@lines;
}

###############################################################################
#
#   Sub Name:       method_summary
#
#   Description:    Create the HTML table for a method-object summary
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $Q        in      CGI       Query object (for HTML stuff)
#                   $server   in      scalar    Name (ident) of server this
#                                                 method is from
#                   $meth     in      ref       RPC::XML::Method (or deriv.)
#                                                 reference
#                   $base_url in      scalar    Base URL to use when making
#                                                 links
#
#   Returns:        text
#
###############################################################################
sub method_summary
{
    my ($Q, $server, $meth, $base_url) = @_;

    return $Q->table({ -width => '100%' },
              $Q->TR({ -valign => 'top' },
                     $Q->td({ -width => '33%' }, $Q->b('Name:')),
                     $Q->td($Q->tt($Q->a({ -href =>
                                           "$base_url&method=" . $meth->name },
                                         $meth->name)))),
              $Q->TR({ -valign => 'top' },
                     $Q->td($Q->b('Version:')),
                     $Q->td($Q->tt($meth->version))),
              $Q->TR({ -valign => 'top' },
                     $Q->td($Q->b('Hidden status:')),
                     $Q->td($Q->tt($meth->hidden() ? 'Hidden' : 'Visible'))),
              $Q->TR({ -valign => 'top' },
                     $Q->td($Q->b('Calls:')),
                     $Q->td($Q->tt($meth->{called} || 0))));
}

###############################################################################
#
#   Sub Name:       method_detail
#
#   Description:    Provide a detailed description and statistics for the
#                   specified method.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $R        in      ref       Apache object reference
#                   $Q        in      CGI       Query object
#                   $flag     in      scalar    If passed and true, means that
#                                                 we are called from with the
#                                                 Apache::Status module
#
#   Globals:        $SERVER_CLASS
#
###############################################################################
sub method_detail
{
    my ($self, $R, $Q, $flag) = @_;
    # $flag has no relevance in this routine

    my ($server, $srv, $method, $meth, $version, $help, @lines);

    $server = $Q->param('server');
    $method = $Q->param('method');

    if (! $server)
    {
        return [ 'Error: No server name specified when screen invoked' ];
    }
    elsif (! ref($srv = $SERVER_CLASS->get_server($server)))
    {
        return [ "Error fetching server named $server: $srv" ];
    }
    if (! $method)
    {
        return [ 'Error: No method name specified when screen invoked' ];
    }
    elsif (! ref($meth = $srv->get_method($method)))
    {
        return [ "Error: No method named $method found on server $server" ];
    }

    push @lines, '<div align="center">', $Q->b('Method: '), $Q->tt($method);
    push @lines, $Q->br(), $Q->br();
    push @lines, '<table border="0" width="75%">';
    if ($version = $meth->version)
    {
        push @lines, $Q->TR({ -valign => 'top' },
                            $Q->td($Q->b('Version:')),
                            $Q->td($Q->tt($version)));
    }
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td({ -width => '30%' }, $Q->b('Hidden from API:')),
                        $Q->td($Q->tt($meth->hidden() ? 'Yes' : 'No')));
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('Calls:')),
                        $Q->td($Q->tt($meth->{called})));
    if ($meth->{file})
    {
        push @lines, $Q->TR({ -valign => 'top' },
                            $Q->td($Q->b('Loaded from:')),
                            $Q->td($Q->tt($meth->{file})));
        push @lines, $Q->TR({ -valign => 'top' },
                            $Q->td($Q->b('File last updated:')),
                            $Q->td($Q->tt(scalar localtime $meth->{mtime})));
    }
    push @lines, $Q->TR({ -valign => 'top' },
                        $Q->td($Q->b('Signatures:')),
                        $Q->td($Q->tt(join '<br>' => @{$meth->signature})));
    if ($help = $meth->help)
    {
        push @lines, $Q->TR($Q->td({ -colspan => 2 }, $Q->b('Help string:')));
        push @lines, $Q->TR($Q->td({ -colspan => 2 }, $Q->pre($Q->tt($help))));
    }
    push @lines, '</table></div>';

    return \@lines;
}

1;

__END__

=head1 NAME

Apache::RPC::Status - A status monitor similar to Apache::Status for RPC

=head1 SYNOPSIS

    # In httpd.conf:
    </Location /rpc-status>
        SetHandler perl-script
        PerlHandler Apache::RPC::Status
    </Location>

    # In the start-up Perl file:
    use Apache::RPC::Status;

=head1 DESCRIPTION

The B<Apache::RPC::Status> package is provided as a simple status monitor for
XML-RPC servers running in a B<mod_perl> environment, using the
B<Apache::RPC::Server> class (or derivative of). Patterned after the status
system provided with B<mod_perl> itself, information is broken down into a
series of screens providing information ranging from the RPC servers currently
configured down to the individual methods provided by the servers.

=head2 Information Screens

There are three basic screens provided by the stock B<Apache::RPC::Status>
package:

=over 4

=item Main: Listing of Servers

This screen is the first screen that comes up when the location for which this
class was assigned as a handler is invoked. It lists the server objects that
this running Apache process knows of. Note that if the servers are defined in
such a way as to mean on-demand creation, then a given child process may not
have all the configured servers in memory. This is by design, it is not a
bug. See L<Apache::RPC::Server/Usage Within E<lt>PerlE<gt> Sections> for
details on configuring the RPC servers such that they are pre-loaded into all
child processes.

=item Server: Details of a Server

Each of the known servers in the main screen links to this screen, which
provides details on the specific server. Information such as when the server
was started (which usually matches the time that Apache was started), when the
specific child was started (which may not be the same), number of requests
servered, and so forth is provided. Additionally, each of the methods that the
server provides is listed in alphanumeric order, with a link to the next
screen.

=item Method: Details of a Specific Method

For each of the known methods published by a server, this screen summarizes
all that is known about the method itself. The signatures, help text and
hidden status (whether the method is visible to the introspection API that is
shipped with B<RPC::XML::Server>) are all shown. Some optional information is
shown if available: if the method has a version number associated with it,
that is displayed. If the method was loaded from an external XPL file, the
file path and modification-time are also displayed.

=back

The primary purpose of this status system is to allow for checking the
availability and sanity of the RPC servers themselves. For example, if a
server is configured to auto-load methods, and automatically check for
updates, the status system could confirm that a method is available or is at
the correct version.

(Note that auto-loading and auto-updating are done on demand, when a call is
made to the method in question. Thus, the status might not reflect changes
until at least one call has been made. Further, if there are very many child
processes handling the RPC servers, several calls may be necessary to ensure
that the child process answering the status request also has the most
up-to-date impression of the server.)

=head1 SUBROUTINES/METHODS

This package is implemented as a method handler for Apache/mod_perl. This
means that is should be relatively easy to subclass this package to implement
an extended version of status reporting, or to provide handlers for phases of
the request lifecycle not otherwise addressed.

=head2 Class Methods

There are three class methods defined in this package. One is the constructor,
the other two are handlers for specific phases in the Apache request
lifecycle.

=over 4

=item new(CLASS, ARGS)

This creates a new object of this class and returns a reference to it. The
first argument is the class being created into, the remaining arguments are
treated as key/value pairs (note: not a hash reference). At present, the only
additional argument recognized is:

=over 8

=item serverclass

This is used when the status monitor is being used with a server class other
than B<Apache::RPC::Server> directly. Because several methods from that class
are invoked, it is presumed that the class named here is a subclass of
B<Apache::RPC::Server>. If not, the status monitor may not work correctly, or
at all. In the absence of this value, C<Apache::RPC::Server> is assumed. This
value may also be set with the mod_perl B<PerlSetVar> directive. See the
documentation for C<init_handler>, below.

=back

=item handler(CLASS, REQUEST)

This is the primary entry-point for the package. This is the handler defined
for assignment to C<PerlHandler> in a location configuration block. It is
invoked by mod_perl as a method handler, thus the first argument is either the
name of the class (in the case of class-method, or static, invocation) or the
object configured as the handler. The second argument is the Apache request
object itself.

This method derives the query parameters for the request from the Apache
object, and treats them according to the type of information screen requested:

=over 8

=item screen

This specifies which screen of the status monitor is to be displayed. In
absence, the value defaults to "main", which is the internal identifier for
the primary screen of the status monitor system. If the value of this
parameter does not match a known interface hook, then the handler will signify
to mod_perl that it cannot handler the request, by replying with the
C<B<DECLINED>> response code.

=item server

When the B<screen> parameter is set to C<server>, the monitor displays the
server detail screen. In that case, this parameter specifies which server
should be displayed. Servers are given unique identifiers when they are
created, usually derived from the URL path that they are attached to. If the
value here does not match any known servers, a warning is sent to the browser.

=item method

When the B<screen> parameter is set to C<method>, this calls for the method
detail screen. The provided interface hook to deal with these requests looks
for both the B<server> parameter above and this one, which specifies by name
the method to be laid out in detail. As with the B<server> parameter, if the
value in this parameter does not match any known data, an error is reported to
the browser.

=back

Any additional parameters will be preserved by B<make_url> call detailed
below. These are merely the specific ones recognized by the status monitor as
written.

=item init_handler(CLASS, REQUEST)

This is a very simple handler designed for the B<PerlChildInitHandler>
phase. At present, it only does one simple task (and thus makes no direct use
of either parameter passed to it by mod_perl). However, it is included mainly
as a placeholder for possible future expansion. The current behavior is to
check for the existence of directory-configuration item called C<ServerClass>,
and record the value if it is set. This is used to specifiy the class from
which the RPC server objects are created, if something other than
B<Apache::RPC::Server>. If this information is passed via the C<serverclass>
parameter to the B<new> method above, that value overrides any value
here. However, that requires actually creating an object to use as the
handler, whereas this handler may be used directly, as a static handler. It
would be configured outside of any E<lt>LocationE<gt> blocks, a requirement
for the B<PerlChildInitHandler> phase. It is designed to stack cleanly with
any other handlers for that phase, provided your mod_perl installation
supports stacked handlers.

=back

=head2 Additional Methods

In addition to the class methods above, the following are provided. In most
cases, these do not rely on any data contained within the actual object
itself. Many may also be called as static methods (these are so noted). They
are provided as a utility, implemented as methods so as to avoid namespace
issues:

=over 4

=item version

(May be called as a static method.) Returns the current version of this
module.

=item apache_status_attach

Attach the B<Apache::RPC::Status> module to the main screen of the
B<Apache::Status> display.

=item default_object

(May be called as a static method.) Returns a default B<Apache::RPC::Status>
instance when called as a static method. Returns the calling reference itself,
otherwise.

=item header(REQUEST, TITLE)

Produces the HTML header for a page. Uses the passed-in title parameter to
give the page a title, and extracts any request-specific information from the
B<Apache> request object passed as the first parameter.

=item footer(REQUEST)

Produces the HTML footer.

=item make_url(QUERY|REQUEST, FLAG)

(May be called as a static method.) This creates a URL string for use as a
hyperlink. It makes certain to preserve all parameters in a CGI-like
fashion. Additionally, it can make the URL in such a fashion as to allow
better integration with the B<Apache::Status> package. If the C<FLAG>
parameter is passed and is any true value, then the resulting URL will be
tailored for use with B<Apache::Status>. The first argument must be either the
original request object as passed by mod_perl, or a reference to a CGI object
created from the request (see L<CGI|CGI> for more on the CGI class).

=item main_screen(REQUEST, QUERY, INTERNAL)

Renders the HTML (minus the header and footer) for the main screen. The
arguments are the B<Apache> request object, a B<CGI> query object created
from the request, and a boolean flag indicating whether the call into this
method was made from within this module or made from the B<Apache::Status>
page.

=item server_summary(SERVER)

Creates an HTML snippet to provide a summary for the server passed in as an
argument. The passed-in value should be the server object, not the name.

=item server_detail(REQUEST, QUERY, INTERNAL)

Renders the HTML (minus header and footer) for a screen describing a server
instance in detail. The server is specified by name in the query parameters.
The arguments are the same as for C<main_screen>.

=item method_summary(SERVER, METHOD, BASEURL)

Creates and HTML snippet to provide a summary for the specified method of the
specified server. The third argument is a base-URL to use for making links to
the detailed method page.

=item method_detail(REQUEST, QUERY, INTERNAL)

Renders the HTML (minus header and footer) for a screen describing a method on
a specific server instance, in detail. The method and server are specified by
name in the query parameters. The arguments are the same as for
C<main_screen>.

=back

=head2 Use and Extension Within Perl Sections

Some extension may be done without necessarily subclassing this package. The
class object are implemented simply as hash references. When a request is
received, the B<screen> parameter (see above) is extracted, and used to look
up in the hash table. If there is a value for that key, the value is assumed
to be a hash reference with at least two keys (described below). If it does
not exist, the handler routine declines to handle the request. Thus, some
degree of extension may be done without the need for developing a new class,
if the configuration and manipulation are done within E<lt>PerlE<gt>
configuration blocks.

Adding a new screen means writing a routine to handle the requests, and then
adding a hook into that routine to the object that is the handler for the
Apache location that serves RPC status requests. The routines that are written
to handle a request should expect four arguments (in order):

=over 4

=item The object reference for the location handler

=item The Apache request object reference

=item A query object reference (see below)

=item A flag that is only passed when called from Apache::Status

=back

The routines are given both the original request object and a query object
reference for sake of ease. The query object is already available prior to the
dispatch, so there is no reason to have each hook routine write the same few
lines to derive a query object from an Apache request. At the same time, the
hooks themselves may need the Apache object to call methods on. The query
object is an instance of B<CGI>. The flag parameter is passed by the linkage
from this status package to B<Apache::Status>. The primary use for it is to
pass to routines such as B<make_url> that are sensitive to the
B<Apache::Status> context.

The return value from these routines must be a reference to a list of lines of
text. It is passed to the B<print> method of the B<Apache> class. This is
necessary for compatibility with the B<Apache::Status> environment.

To add a new hook, merely assign it to the object directly. The key is the
value of the C<screen> parameter defined above, and the value is a hash
reference with two keys:

=over 4

=item title

A string that is incorporated into the HTML title for the page.

=item call

A reference to a subroutine or closure that implements the hook, and conforms
to the conventions described above.

=back

A sample addition:

    $stat_obj->{dbi} = {
                           title => 'RPC-side DBI Pool',
                           call  => \&show_dbi_pool
                       };

=head1 INTEGRATION WITH Apache::Status

This package is designed to integrate with the B<Apache::Status> package that
is a part of mod_perl. However, this is not currently functional. When this
has been debugged, the details will be presented here.

=head1 CAVEATS

This is the newest part of the RPC-XML package. While the package as a whole
is now considered beta, this piece may yet undergo some alpha-like
enhancements to the interface and such. However, the design and planning of
this were carefully considered, so any such changes should be minimal.

=head1 DIAGNOSTICS

Diagnostics are not handled well in this module.

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

L<Apache::Status|Apache::Status>, L<Apache::RPC::Server|Apache::RPC::Server>,
L<RPC::XML::Method|RPC::XML::Method>

=head1 AUTHOR

Randy J. Ray C<< <rjray@blackperl.com> >>

=cut

