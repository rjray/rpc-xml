#!/usr/bin/perl

# Test the RPC::XML::Method class

use strict;
use vars qw($obj $obj2 $flag $dir $tmp);

use File::Spec;
use Test;

use RPC::XML::Procedure;

BEGIN { plan tests => 25 }

(undef, $dir, undef) = File::Spec->splitpath($0);

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# any other classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Method. Thus, we are not testing any
# part of the RPC::XML::Server class here. Only the code for managing methods.

# Tests 1-3: Basic new() success, simple accessors and successful calling
$obj = RPC::XML::Method->new({ name      => 'test.test',
                               signature => [ 'int' ],
                               code      => sub { $flag = 1; } });
if (ref($obj) eq 'RPC::XML::Method')
{
    ok(1); # Signal that one passed
    ok(($obj->name() eq 'test.test') &&
       (scalar(@{$obj->signature}) == 1) &&
       ($obj->signature->[0] eq 'int'));
    $flag = 0;
    eval { $obj->code->(); };
    ok(! $@ and $flag);
    ok($obj->is_valid);
}
else
{
    ok(0); # Signal failure
    ok(0); # These two cannot run if the first one fails
    ok(0);
    ok(0);
}

# 5: This should fail due to missing information (the signature)
$obj = RPC::XML::Method->new({ name => 'test.test2',
                               code => sub { $flag = 2; } });
ok(! ref($obj));

# 6: This one fails because the signatures have a collision
$obj = RPC::XML::Method->new({ name      => 'test.test2',
                               signature => [ 'int int',
                                              'string int' ],
                               code      => sub { $flag = 2; } });
ok(! ref($obj));

# 7: This file will not load due to missing required information
$obj = RPC::XML::Method->new(File::Spec->catfile($dir, 'meth_bad_1.xpl'));
ok(! ref($obj) and $obj =~ /missing/i);

# 8: This file will not load due to an XML parsing error
$obj = RPC::XML::Method->new(File::Spec->catfile($dir, 'meth_bad_2.xpl'));
ok(! ref($obj) and $obj =~ /error parsing/i);

# 9: And the third bowl of porridge was _just_ _right_...
$obj = RPC::XML::Method->new(File::Spec->catfile($dir, 'meth_good_1.xpl'));
ok(ref $obj);

# 10: Check the basics
ok(ref($obj) and $obj->name() and scalar(@{$obj->signature}) and
   $obj->version() and $obj->help());

# 11: Is code() the type of ref we expect?
ok(ref($obj) and (ref($obj->code) eq 'CODE'));

# 12: This looks more complex than it is. The code returns this specific key:
ok($obj->code->({ method_name => $obj->name }) eq $obj->name);

# Time to test cloning
$obj2 = $obj->clone;

# 13: Did it?
ok(ref($obj2) eq ref($obj));
# 14: Primary accessors/data
ok(($obj->name()    eq $obj2->name())    and
   ($obj->version() eq $obj2->version()) and
   ($obj->help()    eq $obj2->help()));
# 15: Are the actual listrefs of signatures different?
ok($obj->signature() ne $obj2->signature());
# 16: And yet, the contents are the same?
ok((@{$obj->signature} == @{$obj2->signature}) and
   # There's only one signature in the list
   ($obj->signature->[0] eq $obj2->signature->[0]));
# 17: Lastly, and very importantly, the coderefs are still the same
ok($obj->code() eq $obj2->code());

undef $obj2; # Don't need it anymore

# Now let's play around with signatures a bit

# 18: Basic test of match_signature()
ok($obj->match_signature('') eq 'string');

# 19: Add a new signature, simple
ok(ref $obj->add_signature('int int'));
# 20: There should now be two
ok(scalar(@{$obj->{signature}}) == 2);
# 21: Does the new one match OK?
ok($obj->match_signature('int') eq 'int');

# 22: This addition should fail due to ambiguity
ok(! ref($tmp = $obj->add_signature([ 'double', 'int' ])));
# 23: But did it fail for the right reasons?
ok($tmp =~ /make_sig_table/);

# 24: Test deletion
ok(ref($obj->delete_signature('int int')));
# 25: Which means checking the count again
ok(scalar(@{$obj->{signature}}) == 1);

exit;
