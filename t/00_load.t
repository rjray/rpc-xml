#!/usr/bin/perl

use strict;
use vars qw(@MODULES);

use Test;

# $Id: 00_load.t,v 1.3 2001/07/18 05:46:41 rjray Exp $
# Verify that the individual modules will load

BEGIN
{
    @MODULES = qw(RPC::XML RPC::XML::Parser RPC::XML::Client RPC::XML::Server
                  Apache::RPC::Server);

    # If mod_perl is not available, Apache::RPC::Server cannot be blamed
    eval "use Apache";
    pop(@MODULES) if $@;

    plan tests => scalar(@MODULES);
}

for (@MODULES)
{
    eval "use $_";
    ok(! $@);
}

exit 0;
