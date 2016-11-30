#!/usr/bin/env perl

# Test the data-manipulation routines in RPC::XML

## no critic(RequireInterpolationOfMetachars)

use strict;
use warnings;

use Test::More;

use RPC::XML 'smart_encode';

my $obj;

plan tests => 11;

# First ensure that we can't actually create these objects unless we explicitly
# enable the extension:
$obj = RPC::XML::nil->new();
ok(! defined($obj), 'Did not create a nil without first enabling nil');
like($RPC::XML::ERROR, qr/RPC::XML::ALLOW_NIL must be set/,
     '$RPC::XML::ERROR correctly set');

# Enable and try again
$RPC::XML::ALLOW_NIL = 1;
$obj = RPC::XML::nil->new();
isa_ok($obj, 'RPC::XML::nil');

# Check stringification and length
is($obj->as_string, '<nil/>', 'Stringification');
is($obj->length, 6, 'Length of element');

# Test the convenience function
{
    use RPC::XML 'RPC_NIL';

    isa_ok(RPC_NIL, 'RPC::XML::nil');
}

# Verify that anything passed to the constructor has no effect on the created
# object:
$obj = RPC::XML::nil->new('ignored');
isa_ok($obj, 'RPC::XML::nil');
is($obj->as_string, '<nil/>', 'Stringification');
is($obj->length, 6, 'Length of element');

# With nil enabled, smart_encode() should now encode undef as a nil, not as a
# null-length string:
$obj = smart_encode(undef);
is($obj->type, 'nil', 'smart_encode undef->string type');
is($obj->value, undef, 'smart_encode undef->string value');

exit 0;
