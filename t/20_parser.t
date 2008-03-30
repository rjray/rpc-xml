#!/usr/bin/perl
# $Id$

# Test the XML::Parser container

use strict;
use vars qw($p $req $res $ret $dir $file);

use Test::More tests => 14;
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
