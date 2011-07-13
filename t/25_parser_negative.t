#!/usr/bin/perl

# Test the RPC::XML::Parser class negative conditions

use strict;
use warnings;
use vars qw($p $retval);

use Test::More tests => 14;

# Create a dummy class to use for attempts to call methods within the
# RPC::XML::Parser class:
package BadParser;

use base 'RPC::XML::Parser';

package main;

# This is the pattern we are looking for in the error messages:
my $errtext = qr/should have been overridden by the BadParser class/;

# First, the constructor:
eval { $p = BadParser->new() };
ok(! defined $p, 'RPC::XML::Parser did not instantiate');
like($@, $errtext, 'Correctly-set error message in $@');

# Fine! We'll *force* an object into that class:
$p = bless {}, 'BadParser';

# *Now* try and stop me from calling methods!
$retval = eval { $p->parse(); 1 };
ok(! $retval, '::parse correctly failed to run');
like($@, $errtext, 'Correctly-set error message in $@');

$retval = eval { $p->parse_more(); 1 };
ok(! $retval, '::parse_more correctly failed to run');
like($@, $errtext, 'Correctly-set error message in $@');

$retval = eval { $p->parse_done(); 1 };
ok(! $retval, '::parse_done correctly failed to run');
like($@, $errtext, 'Correctly-set error message in $@');

# Try them as static methods:
$retval = eval { BadParser->parse(); 1 };
ok(! $retval, '::parse correctly failed to run');
like($@, $errtext, 'Correctly-set error message in $@');

$retval = eval { BadParser->parse_more(); 1 };
ok(! $retval, '::parse_more correctly failed to run');
like($@, $errtext, 'Correctly-set error message in $@');

$retval = eval { BadParser->parse_done(); 1 };
ok(! $retval, '::parse_done correctly failed to run');
like($@, $errtext, 'Correctly-set error message in $@');

exit;
