#!/usr/bin/env perl

# Test whether the client and server classes correctly detect the presence
# or absence of compression support.

use strict;
use warnings;

use Module::Load;
use Symbol 'delete_package';

use Test::More;

# These are the modules that need to correctly detect compression:
our %TEST_PKGS = ('RPC::XML::Client' => 'RPC/XML/Client.pm',
                  'RPC::XML::Server' => 'RPC/XML/Server.pm');

plan tests => (2 * (scalar keys %TEST_PKGS));

# This will prevent Compress::Zlib from loading, regardless of whether it is
# available:
unshift @INC, sub {
    die "Force-failing Compress::Zlib\n" if ($_[1] eq 'Compress/Zlib.pm');
    return;
};

for my $pkg (sort keys %TEST_PKGS)
{
    # Needed to soft-deref ${pkg}::COMPRESSION_AVAILABLE
    ## no critic (ProhibitNoStrict)
    no strict 'refs';

    load $pkg;

    is(${"${pkg}::COMPRESSION_AVAILABLE"}, q{},
       "$pkg correctly saw no Compress::Zlib");

    # Remove from %INC so later tests still run
    clear($pkg, $TEST_PKGS{$pkg});
}

# Determine if we actually *do* have Compress::Zlib available:
shift @INC; # First drop the force-failure sub from above
my $compression_available = eval { load Compress::Zlib; 1; };

SKIP: {
    # Test successful detection, but only if we actually have Compress::Zlib
    if (! $compression_available)
    {
        skip 'Compress::Zlib truly not available', (scalar keys %TEST_PKGS);
    }

    for my $pkg (sort keys %TEST_PKGS)
    {
        # Needed to soft-deref ${pkg}::COMPRESSION_AVAILABLE
        ## no critic (ProhibitNoStrict)
        no strict 'refs';

        load $pkg;

        # I am not explicitly testing for "deflate" here, because that might
        # change in the future. What matters is that it is not null.
        isnt(${"${pkg}::COMPRESSION_AVAILABLE"}, q{},
           "$pkg correctly detected Compress::Zlib");
    }
}

exit;

sub clear
{
    my ($pkg, $file) = @_;

    delete $INC{$file};
    delete_package($pkg);

    return;
}
