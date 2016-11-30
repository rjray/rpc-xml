#!/usr/bin/env perl

# https://rt.cpan.org/Ticket/Display.html?id=54494
#
# Test that smart_encode() in RPC::XML can correctly deal with blessed refs
# by treating them as non-blessed.

## no critic(RequireInterpolationOfMetachars)

use strict;
use warnings;

use Test::More;

use RPC::XML ':all';

plan tests => 8;

my ($val, $obj, $result);

$val = bless { integer => 10, string => 'foo' }, 'BlessedHash';
$result = eval { $obj = smart_encode($val); 1; };
isa_ok($obj, 'RPC::XML::struct', '$obj');
SKIP: {
    if (ref($obj) ne 'RPC::XML::struct')
    {
        skip 'Blessed hash did not encode', 2;
    }

    my $value = $obj->value;
    is($value->{integer}, 10, 'Converted hash integer value');
    is($value->{string}, 'foo', 'Converted hash string value');
}

$val = bless [ 10, 'foo' ], 'BlessedArray';
$result = eval { $obj = smart_encode($val); 1; };
isa_ok($obj, 'RPC::XML::array', '$obj');
SKIP: {
    if (ref($obj) ne 'RPC::XML::array')
    {
        skip 'Blessed array did not encode', 2;
    }

    my $value = $obj->value;
    is($value->[0], 10, 'Converted array integer value');
    is($value->[1], 'foo', 'Converted array string value');
}

$val = bless \do { my $elt = 'foo' }, 'BlessedScalar';
$result = eval { $obj = smart_encode($val); 1; };
isa_ok($obj, 'RPC::XML::string', '$obj');
SKIP: {
    if (ref($obj) ne 'RPC::XML::string')
    {
        skip 'Blessed scalar did not encode', 1;
    }

    my $value = $obj->value;
    is($value, 'foo', 'Converted scalar value');
}

exit;
