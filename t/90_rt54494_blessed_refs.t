#!/usr/bin/perl

# https://rt.cpan.org/Ticket/Display.html?id=54494
#
# Test that smart_encode() in RPC::XML can correctly deal with blessed refs
# by treating them as non-blessed.

use strict;
use vars qw($val $obj);

use Test::More tests => 8;

use RPC::XML ':all';

$val = bless { integer => 10, string => 'foo' }, 'BlessedHash';
eval { $obj = smart_encode($val); };
isa_ok($obj, 'RPC::XML::struct', '$obj');
SKIP: {
    skip 'Blessed hash did not encode', 2
        unless (ref($obj) eq 'RPC::XML::struct');

    my $value = $obj->value;
    is($value->{integer}, 10, 'Converted hash integer value');
    is($value->{string}, 'foo', 'Converted hash string value');
}

$val = bless [ 10, 'foo' ], 'BlessedArray';
eval { $obj = smart_encode($val); };
isa_ok($obj, 'RPC::XML::array', '$obj');
SKIP: {
    skip 'Blessed array did not encode', 2
        unless (ref($obj) eq 'RPC::XML::array');

    my $value = $obj->value;
    is($value->[0], 10, 'Converted array integer value');
    is($value->[1], 'foo', 'Converted array string value');
}

$val = bless \do { my $elt = 'foo' }, 'BlessedScalar';
eval { $obj = smart_encode($val); };
isa_ok($obj, 'RPC::XML::string', '$obj');
SKIP: {
    skip 'Blessed scalar did not encode', 1
        unless (ref($obj) eq 'RPC::XML::string');

    my $value = $obj->value;
    is($value, 'foo', 'Converted scalar value');
}

exit;
