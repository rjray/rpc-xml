#!/usr/bin/perl

# Test the XML::Parser container

use strict;
use vars qw($p $req $res $ret);

use Test;

use RPC::XML ':all';
use RPC::XML::Parser;

BEGIN { plan tests => 7 }

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# RPC::XML::* classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Parser.

$p = new RPC::XML::Parser;
ok(ref $p);

$req = new RPC::XML::request 'test.method';
$ret = $p->parse($req->as_string);
ok(ref($ret) && $ret->isa('RPC::XML::request'));
ok($ret->name, 'test.method');

$res = new RPC::XML::response (new RPC::XML::string 'test response');
$ret = $p->parse($res->as_string);
ok(ref($ret) && $ret->isa('RPC::XML::response'));
ok($ret->value->value, 'test response');

# Test some badly-formed data
my $tmp = $res->as_string; $tmp =~ s/methodResponse/mR/g;
$ret = $p->parse($tmp);
ok(! ref($ret));
ok($ret =~ /Unknown tag/);

exit 0;
