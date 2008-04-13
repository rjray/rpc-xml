#!/usr/bin/perl
# $Id$

use strict;
use vars qw(@MODULES @APACHE_MODULES $do_apache);

use Test::More;

# Verify that the individual modules will load

BEGIN
{
    @MODULES = qw(RPC::XML RPC::XML::Parser
                  RPC::XML::Procedure RPC::XML::Method
                  RPC::XML::Client RPC::XML::Server);
    @APACHE_MODULES = qw(Apache::RPC::Server Apache::RPC::Status);

    # If mod_perl is not available, Apache::RPC::Server cannot be blamed
    eval "use Apache";
    $do_apache = $@ ? 0 : 1;

    plan tests => (scalar(@MODULES) + scalar(@APACHE_MODULES));
}

# Core modules
use_ok($_) for (@MODULES);

# Test these only if Apache (v1) is available
SKIP: {
    skip "No mod_perl 1.X detected", scalar(@APACHE_MODULES) unless $do_apache;

    use_ok($_) for (@APACHE_MODULES);
}

exit 0;
