#!/usr/bin/perl
# $Id$

# Test the XML::Parser container

use strict;
use vars qw($p $req $res $ret $dir $file);

use Test;
require File::Spec;
require IO::File;

use RPC::XML ':all';
use RPC::XML::Parser;

BEGIN { plan tests => 13 }

(undef, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$file = File::Spec->catfile($dir, 'svsm_text.gif');

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# RPC::XML::* classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Parser.

$p = RPC::XML::Parser->new();
ok(ref $p);

$req = RPC::XML::request->new('test.method');
$ret = $p->parse($req->as_string);
ok(ref($ret) && $ret->isa('RPC::XML::request'));
ok($ret->name, 'test.method');

$res = RPC::XML::response->new(RPC::XML::string->new('test response'));
$ret = $p->parse($res->as_string);
ok(ref($ret) && $ret->isa('RPC::XML::response'));
ok($ret->value->value, 'test response');

# Test some badly-formed data
my $tmp = $res->as_string; $tmp =~ s/methodResponse/mR/g;
$ret = $p->parse($tmp);
ok(! ref($ret));
ok($ret =~ /Unknown tag/);

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
$ret = $p->parse($req->as_string);
ok(ref($ret) && $ret->isa('RPC::XML::request'));
ok($ret->name, 'method');
ok(ref($ret->args));

my $new_base64 = $ret->args->[0];
ok(ref($new_base64), 'RPC::XML::base64');
ok($base64->as_string(), $new_base64->as_string);
ok(UNIVERSAL::isa($new_base64->{value_fh}, 'GLOB'));

exit 0;
