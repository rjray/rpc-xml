#!/usr/bin/perl

# Verify that the individual modules will load

@MODULES = qw(RPC::XML RPC::XML::Parser RPC::XML::Client RPC::XML::Server);

printf "1..%d\n", scalar(@MODULES);

$count = 0;
for (@MODULES)
{
    eval "use $_";

    printf "%sok %d\n", ($@) ? 'not ' : '', ++$count;
}

exit 0;
