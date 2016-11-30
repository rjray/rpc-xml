#!/usr/bin/env perl

# Test the changes in RPC::XML to prevent infinite recursion on cyclic refs

use strict;
use warnings;

use Test::More;

use RPC::XML 'smart_encode';

my ($val, $newval, $obj);

plan tests => 17;

$val = [ 'a' ];
push @{$val}, $val;
$obj = smart_encode($val);
isa_ok($obj, 'RPC::XML::array');
$newval = $obj->value;
is(scalar(@{$newval}), 1, 'Cyclical array now length 1');
is($newval->[0], 'a', 'Cyclical array kept non-cyclic element');

$obj = RPC::XML::array->new($val);
isa_ok($obj, 'RPC::XML::array');
$newval = $obj->value;
# Because we used the constructor, the first level didn't count for the cyclic
# tests. Instead, the first element is the potentially-cyclical array.
$newval = $newval->[0];
is(scalar(@{$newval}), 1, 'Cyclical array <2> now length 1');
is($newval->[0], 'a', 'Cyclical array <2> kept non-cyclic element');

$val = {};
$val->{a} = 'a';
$val->{b} = [ qw(a b c) ];
$val->{c} = 1;
$val->{b}->[1] = $val;
$obj = smart_encode($val);
isa_ok($obj, 'RPC::XML::struct');
$newval = $obj->value;
is(scalar(keys %{$newval}), 3, 'Cyclical struct has correct num of keys');
is(scalar(@{$newval->{b}}), 2, 'Cyclical struct array elem is correct');
is($newval->{a}, 'a', 'Cyclical struct other key no. 1 correct');
is($newval->{c}, 1, 'Cyclical struct other key no. 2 correct');

$obj = RPC::XML::struct->new($val);
isa_ok($obj, 'RPC::XML::struct');
$newval = $obj->value;
is(scalar(keys %{$newval}), 3, 'Cyclical struct <2> has correct num of keys');
is(scalar(@{$newval->{b}}), 3, 'Cyclical struct <2> array elem is correct');
is($newval->{a}, 'a', 'Cyclical struct <2> other key no. 1 correct');
is($newval->{c}, 1, 'Cyclical struct <2> other key no. 2 correct');
is(scalar(keys %{$newval->{b}->[1]}), 2,
   'Cyclical struct <2> nested hash has correct keys');

exit 0;
