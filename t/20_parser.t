#!/usr/bin/perl

# Test the XML::Parser container

use strict;
use vars qw($p $req $res $ret $dir $file);

use Test::More tests => 35;
require File::Spec;
require IO::File;

use RPC::XML ':all';
use RPC::XML::Parser;

(undef, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$file = File::Spec->catfile($dir, 'svsm_text.gif');

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# RPC::XML::* classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Parser.

$p = RPC::XML::Parser->new();
isa_ok($p, 'RPC::XML::Parser', '$p');

$req = RPC::XML::request->new('test.method');
$ret = $p->parse($req->as_string);
isa_ok($ret, 'RPC::XML::request', '$ret');
is($ret->name, 'test.method', 'Correct request method name');

$res = RPC::XML::response->new(RPC::XML::string->new('test response'));
$ret = $p->parse($res->as_string);
isa_ok($ret, 'RPC::XML::response', '$ret');
is($ret->value->value, 'test response', 'Response value');

# Test some badly-formed data
my $tmp = $res->as_string; $tmp =~ s/methodResponse/mR/g;
$ret = $p->parse($tmp);
ok(! ref($ret), 'Bad XML did not parse');
like($ret, qr/Unknown tag/, 'Parse failure returned error');

# Make sure that the parser can handle all of the core data-types. Easiest way
# to do this is to create a fake request with a parameter of each type (except
# base64, which is getting exercised later on).
$req = RPC::XML::request->new(
    'parserTest',
    RPC::XML::i4->new(1),
    RPC::XML::int->new(2),
    RPC::XML::i8->new(3),
    RPC::XML::double->new(4.5),
    RPC::XML::string->new('string'),
    RPC::XML::boolean->new('true'),
    RPC::XML::datetime_iso8601->new('2008-09-29T12:00:00-07:00'),
    [ 0, 1 ], # Array, auto-encoded
    { a => 1, b => 2 }, # Hash/struct, also auto-encoded
);
$ret = $p->parse($req->as_string);
isa_ok($ret, 'RPC::XML::request', 'Parse of RPC::XML::request block');
SKIP: {
    skip "RPC::XML::request object not properly parsed, cannot test it.", 20
        unless (ref($ret) eq 'RPC::XML::request');

    is($ret->name, 'parserTest', 'Properly parsed /methodCall/methodName');
    my $args = $ret->args;
    is(scalar @$args, 9, 'Parser created correct-length args list');
    # I could (and should) probably turn this into a loop with a table of
    # data, but I'm lazy right this moment.
    isa_ok($args->[0], 'RPC::XML::i4', 'Parse of <i4> argument');
    is($args->[0]->value, 1, 'RPC::XML::i4 value parsed OK');
    isa_ok($args->[1], 'RPC::XML::int', 'Parse of <int> argument');
    is($args->[1]->value, 2, 'RPC::XML::int value parsed OK');
    isa_ok($args->[2], 'RPC::XML::i8', 'Parse of <i8> argument');
    is($args->[2]->value, 3, 'RPC::XML::i8 value parsed OK');
    isa_ok($args->[3], 'RPC::XML::double', 'Parse of <double> argument');
    is($args->[3]->value, 4.5, 'RPC::XML::double value parsed OK');
    isa_ok($args->[4], 'RPC::XML::string', 'Parse of <string> argument');
    is($args->[4]->value, 'string', 'RPC::XML::string value parsed OK');
    isa_ok($args->[5], 'RPC::XML::boolean', 'Parse of <boolean> argument');
    ok($args->[5]->value, 'RPC::XML::boolean value parsed OK');
    isa_ok($args->[6], 'RPC::XML::datetime_iso8601',
           'Parse of <dateTime.iso8601> argument');
    is($args->[6]->value, '2008-09-29T12:00:00-07:00',
       'RPC::XML::dateTime.iso8601 value parsed OK');
    isa_ok($args->[7], 'RPC::XML::array', 'Parse of <array> argument');
    is(scalar(@{$args->[7]->value}), 2, 'RPC::XML::array value parsed OK');
    isa_ok($args->[8], 'RPC::XML::struct', 'Parse of <struct> argument');
    is(scalar(keys %{$args->[8]->value}), 2,
       'RPC::XML::struct value parsed OK');
}

# Prior to this, we've confirmed that spooling base64 data to files works.
# Here, we test whether the parser (when configured to do so) can create
# filehandles as well.
undef $p;
$p = RPC::XML::Parser->new(base64_to_fh => 1);
my $fh = IO::File->new("< $file");
die "Error opening $file: $!" unless ref $fh;
my $base64 = RPC::XML::base64->new($fh);
$req = RPC::XML::request->new('method', $base64);

# Start testing
my $spool_ret = $p->parse($req->as_string);
isa_ok($spool_ret, 'RPC::XML::request', '$spool_ret');
is($spool_ret->name, 'method', 'Request, base64 spooling, method name test');
ok(ref($spool_ret->args), 'Request, base64 spooling, return arg test');

my $new_base64 = $spool_ret->args->[0];
isa_ok($new_base64, 'RPC::XML::base64', '$new_base64');
is($base64->as_string(), $new_base64->as_string,
   'Parse base64 spooling, value comparison');
isa_ok($new_base64->{value_fh}, 'GLOB', '$new_base64->{value_fh}');

# Per problem reported by Bill Moseley, check that messages parsed by the
# parser class handle the core entities.
$tmp = q{Entity test: & < > ' "};
$res = RPC::XML::response->new($tmp);
$ret = $p->parse($res->as_string);
is($ret->value->value, $tmp, 'RPC::XML::Parser handles core entities');

exit 0;
