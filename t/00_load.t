#!/usr/bin/perl

use strict;
use vars qw(@MODULES);

use Test;

# Verify that the individual modules will load

BEGIN
{
    @MODULES = qw(RPC::XML RPC::XML::Parser RPC::XML::Client RPC::XML::Server
                  Apache::RPC::Server);
    plan tests => scalar(@MODULES);
}

for (@MODULES)
{
    eval "use $_";
    ok(! $@);
}

exit 0;
