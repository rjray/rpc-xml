#!/usr/bin/perl

# Test whether the client and server classes correctly detect the presence
# or absence of compression support.

use strict;
use warnings;
use vars qw($compression_available %TEST_PKGS);

use Symbol 'delete_package';

use Test::More;

# First, determine if we actually *do* have Compress::Zlib available:
eval { require Compress::Zlib; };
$compression_available = ($@) ? 0 : 1;

# These are the modules that need to correctly detect compression:
%TEST_PKGS = ( 'RPC::XML::Client' => 'RPC/XML/Client.pm',
               'RPC::XML::Server' => 'RPC/XML/Server.pm' );

plan tests => (2 * (scalar keys %TEST_PKGS));

# Clear out Compress::Zlib so it'll try to load again:
clear('Compress::Zlib', 'Compress/Zlib.pm');

# If compression is truly not available, just test that the modules correctly
# detect this. Otherwise, get crafty.

# Start by testing failure-to-detect
unshift(@INC, sub {
    die "Force-failing Compress::Zlib" if ($_[1] eq 'Compress/Zlib.pm');
    return undef;
}) if ($compression_available);

for my $pkg (sort keys %TEST_PKGS)
{
    no strict 'refs';

    eval "require $pkg;";

    is(${"${pkg}::COMPRESSION_AVAILABLE"}, '',
       "$pkg correctly saw no Compress::Zlib");

    # Remove from %INC so later tests still run
    clear($pkg, $TEST_PKGS{$pkg});
}

SKIP: {
    # Test successful detection, but only if we actually have Compress::Zlib
    skip 'Compress::Zlib truly not available', (scalar keys %TEST_PKGS)
        unless $compression_available;

    shift(@INC); # Drop the force-failure sub from above
    for my $pkg (sort keys %TEST_PKGS)
    {
        no strict 'refs';

        clear('Compress::Zlib', 'Compress/Zlib.pm');
        for (qw(deflate flush inflate))
        {
            clear('Compress::Zlib', 'Compress/Zlib.pm', $_);
        }
        eval "require $pkg;";

        # I am not explicitly testing for "deflate" here, because that might
        # change in the future. What matters is that it is not null.
        isnt(${"${pkg}::COMPRESSION_AVAILABLE"}, '',
           "$pkg correctly detected Compress::Zlib");
    }
}

exit;

sub clear
{
    no strict 'refs';

    my ($pkg, $file, $name) = @_;

    delete $INC{$file};
    delete_package($pkg);
    if ($pkg eq 'Compress::Zlib')
    {
        delete_package 'Zlib::OldDeflate';
        delete_package 'Zlib::OldInflate';
    }
}
