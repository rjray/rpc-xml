#!/usr/bin/perl

# Test the RPC::XML::Method class

use strict;
use warnings;
use vars qw($obj $obj2 $flag $dir $tmp);

use File::Spec;
use Test::More;

use RPC::XML::Procedure;

plan tests => 25;

(undef, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# any other classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Procedure. As such, we are not testing
# any part of the RPC::XML::Server class here. Only the code for managing
# methods.

# Basic new() success, simple accessors and successful calling
$obj = RPC::XML::Procedure->new({ name      => 'test.test',
                                  signature => [ 'int' ],
                                  code      => sub { $flag = 1; } });
isa_ok($obj, 'RPC::XML::Procedure');
SKIP: {
    skip 'Cannot test without object', 3
        unless (ref($obj) eq 'RPC::XML::Procedure');

    ok(($obj->name() eq 'test.test') &&
       (scalar(@{$obj->signature}) == 1) &&
       ($obj->signature->[0] eq 'int'),
       'Basic accessors');
    $flag = 0;
    eval { $obj->code->(); };
    ok((! $@) && $flag, 'Calling the code');
    ok($obj->is_valid(), 'Test is_valid() method');
}

# This should fail due to missing information (the signature)
$obj = RPC::XML::Method->new({ name => 'test.test2',
                               code => sub { $flag = 2; } });
ok(! ref($obj), 'Correct constructor failure [1]');

# This one fails because the signatures have a collision
$obj = RPC::XML::Method->new({ name      => 'test.test2',
                               signature => [ 'int int',
                                              'string int' ],
                               code      => sub { $flag = 2; } });
ok(! ref($obj), 'Correct constructor failure [2]');

# This file will not load due to missing required information
$obj = RPC::XML::Method->new(File::Spec->catfile($dir, 'meth_bad_1.xpl'));
like($obj, qr/missing/i, 'Bad XPL [1] not loaded');

# This file will not load due to an XML parsing error
$obj = RPC::XML::Method->new(File::Spec->catfile($dir, 'meth_bad_2.xpl'));
like($obj, qr/error parsing/i, 'Bad XPL [2] not loaded');

# And the third bowl of porridge was _just_ _right_...
$obj = RPC::XML::Method->new(File::Spec->catfile($dir, 'meth_good_1.xpl'));
isa_ok($obj, 'RPC::XML::Method');

# Check the basics
ok(ref($obj) && $obj->name() && scalar(@{$obj->signature}) &&
   $obj->version() && $obj->help(),
   'Good XPL load, basic accessors');

# Is code() the type of ref we expect?
ok(ref($obj) && (ref($obj->code) eq 'CODE'),
   'Good XPL load, code() accessor');

# This looks more complex than it is. The code returns this specific key:
is($obj->code->({ method_name => $obj->name }), $obj->name(),
   'Good XPL load, code() invocation');

# Time to test cloning
$obj2 = $obj->clone;

# Did it?
isa_ok($obj2, ref($obj));
# Primary accessors/data
ok(($obj->name()    eq $obj2->name())    &&
   ($obj->version() eq $obj2->version()) &&
   ($obj->help()    eq $obj2->help()),
   'Compare accessors of clone and source');
# Are the actual listrefs of signatures different?
isnt($obj->signature(), $obj2->signature(),
     'Clone signature() accessor has different listref');
# And yet, the contents are the same?
ok((@{$obj->signature} == @{$obj2->signature}) &&
   # There's only one signature in the list
   ($obj->signature->[0] eq $obj2->signature->[0]),
   'Clone signature() value is same despite this');
# Lastly, and very importantly, the coderefs are still the same
is($obj->code(), $obj2->code(), 'Clone code() ref value is same as source');

undef $obj2; # Don't need it anymore

# Now let's play around with signatures a bit

# Basic test of match_signature()
is($obj->match_signature(''), 'string', 'Test match_signature()');

# Add a new signature, simple
is($obj->add_signature('int int'), $obj,
   'Adding via add_signature() returns obj ref');
# There should now be two
is(scalar(@{$obj->{signature}}), 2,
   'Number of signatures after add_signature()');
# Does the new one match OK?
is($obj->match_signature('int'), 'int', 'New signature matches correctly');

# This addition should fail due to ambiguity
isnt($tmp = $obj->add_signature([ 'double', 'int' ]), $obj,
     'Correct failure of adding ambiguous signature');
# But did it fail for the right reasons?
like($tmp, qr/make_sig_table/, 'Signature failure returned correct message');

# Test deletion
is($obj->delete_signature('int int'), $obj, 'Test delete_signature()');
# Which means checking the count again
is(scalar(@{$obj->{signature}}), 1, 'Correct signature count after delete');

exit;
