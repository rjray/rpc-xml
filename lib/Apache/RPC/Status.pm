###############################################################################
#
# This file copyright (c) 2001 by Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License as distributed with Perl versions 5.005 and later. See
# http://language.perl.com/misc/Artistic.html
#
###############################################################################
#
#   $Id: Status.pm,v 1.1 2001/08/18 01:07:15 rjray Exp $
#
#   Description:    This module is intended to provide a browser-friendly
#                   status page on the RPC server(s) being managed by the
#                   hosting Apache process.
#
#                   Some parts of this are borrowed from the Apache::Status
#                   module.
#
#   Functions:      handler
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

use 5.005;
use strict;
use vars qw($SERVER_VER $STARTED $PERL_VER %titles %hooks);

use Apache;
use Apache::Constants 'OK';

BEGIN
{
    $SERVER_VER = Apache::Constants::SERVER_VERSION();
    $STARTED    = scalar localtime $^T;
    $PERL_VER   = $^V ? sprintf "v%vd", $^V : $];
}

$Apache::RPC::Status::VERSION = do { my @r=(q$Revision: 1.1 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

my %is_installed = ();
my $Is_Win32 = ($^O eq "MSWin32");
{
    local $SIG{__DIE__};
    %is_installed = map {
        $_, (eval("require $_") || 0);
    } qw(Data::Dumper Devel::Symdump B Apache::Request Apache::Peek
         Apache::Symbol);
}

my $CPAN_base = "http://www.perl.com/CPAN/modules/by-module";

%titles = ( env  => 'Environment Variables',
            sig  => 'Installed Signal Handlers',
            perl => 'Perl-Level Configuration' );

%hooks  = ( env  => \&show_env,
            sig  => \&show_sig_handlers,
            perl => \&show_perlconfig );

1;

#
# ??? Why did I make this a method handler? I remember doing it on purpose
#
# 2001/07/20: Oh yeah, it's so people can sub-class this to extend it.
#
sub handler ($$)
{
    my $class = shift;
    my $r     = shift;

    my ($qs, $pick, %args);

    $qs = $r->args;
    if ($qs =~ /^(\w+)&(.*)/)
    {
        $pick = $1;
        %args = map { split('=', $_, 2) } (split('&', $2));
    }
    else
    {
        $pick = $qs;
    }

    $class->header($r, $titles{$pick});

    $class->footer($r);
    return OK;
}

# Simple token-response method
sub version { $Apache::RPC::Status::VERSION }

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
#   Environment:    None.
#
#   Returns:        void
#
###############################################################################
sub header
{
    my $self = shift;
    my $r = shift;
    my $title = shift;

    $title = " - $title" if $title;
    $title = (ref($self) || $self) . $title;

    $r->send_http_header('text/html');
    $r->print(<<"EOF");
<html>
<head><title>$title</title></head>
<body bgcolor="white">
Perl version <b>$PERL_VER</b> for <b>$SERVER_VER</b> process <b>$$</b>,
running since $STARTED
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
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        void
#
###############################################################################
sub footer
{
    my $self = shift;
    my $r = shift;

    $r->print(sprintf(qq{<hr>\n<table width="100%">\n} .
                      qq{<tr><td><i>%s %s</i></td>} .
                      qq{<td align="right">%s</td></tr>\n</table>\n} .
                      qq{</body>\n</html>\n},
                      (ref($self) || $self), $self->version,
                      scalar(localtime)));
}

###############################################################################
#
#   Sub Name:       show_sig_handlers
#
#   Description:    Produce and return the HTML text that details the current
#                   configuration of the signal handlers
#
#   Arguments:      None.
#
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        text
#
###############################################################################
sub show_sig_handlers
{
    my ($val, $cv);

    my %tbl =
        (map {
            $val = $SIG{$_} || "";
            if ($val and ref $val eq "CODE")
            {
                if ($cv = Apache::Symbol->can('sv_name'))
                {
                    $val = '\&' . $cv->($val);
                }
            }
            $_, $val
        } (keys %SIG));

    [ qq(<table border="0">\n),
      map { qq(<tr><td><code>$_</code></td><td>$tbl{$_}</td></tr>\n) }
      (sort keys %tbl),
      "</table>\n" ];
}

###############################################################################
#
#   Sub Name:       show_perlconfig
#
#   Description:    Output HTML for the Perl configuration itself
#
#   Arguments:      None.
#
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        text
#
###############################################################################
sub show_perlconfig
{
    require Config;

    [ "<pre>\n", Config::myconfig(), "</pre>\n" ];
}

###############################################################################
#
#   Sub Name:       show_env
#
#   Description:    Output the HTML for a table with all the available
#                   environment variables.
#
#   Arguments:      None.
#
#   Globals:        None.
#
#   Environment:    All of it.
#
#   Returns:        text
#
###############################################################################
sub show_env
{
    [ qq{<table border="0" cellspacing="5">\n},
      (map {
          qq{<tr valign="top"><td align="right"><b><tt>$_</tt></b></td>} .
              qq{<td><tt>$ENV{$_}</tt></td></tr>\n}
           } (sort keys %ENV)),
      qq{</table>\n} ];
}
