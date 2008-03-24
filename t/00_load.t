#!/usr/bin/perl
# $Id$

use strict;
use vars qw(@MODULES);

use Test;

# $Id$
# Verify that the individual modules will load

BEGIN
{
    @MODULES = qw(RPC::XML RPC::XML::Parser
                  RPC::XML::Procedure RPC::XML::Method
                  RPC::XML::Client RPC::XML::Server
                  Apache::RPC::Server Apache::RPC::Status);

    # If mod_perl is not available, Apache::RPC::Server cannot be blamed
    eval "use Apache";
    splice(@MODULES, -2) if $@;

    plan tests => scalar(@MODULES);
}

for (@MODULES)
{
    eval "use $_";
    ok(! $@);
}

exit 0;
