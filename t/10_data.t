#!/usr/bin/perl

# Test the data-manipulation routines in RPC::XML

use strict;
use vars qw($val $obj $class %val_tbl @values);

use Test;
use RPC::XML ':all';

BEGIN { plan tests => 109 }

# First, the most basic data-types

%val_tbl = (
            'int'  => int(rand 10000) + 1,
            i4     => int(rand 10000) + 1,
            double => rand 10001,
            string => __FILE__
           );

for (sort keys %val_tbl)
{
    $val = $val_tbl{$_};
    $class = "RPC::XML::$_";
    $obj = $class->new($val);
    ok(ref $obj);
    ok($obj->value, $val);
    ok($obj->as_string, "<$_>$val</$_>");
    ok($obj->type, $_);
    ok(length($obj->as_string), $obj->length);
}

# Another little test for RPC::XML::string, to check encoding
$val = 'Subroutine &bogus not defined at <_> line -NaN';
$obj = RPC::XML::string->new($val);
ok($obj->value, $val);
ok($obj->as_string,
   "<string>Subroutine &amp;bogus not defined at &lt;_&gt; line -NaN</string>");

# Type boolean is a little funky

# Each of these should be OK
for (qw(0 1 yes no tRuE FaLsE))
{
    $val = (/0|no|false/i) ? 0 : 1;
    $obj = new RPC::XML::boolean $_;
    ok(ref $obj);
    ok($obj->value, $val);
    ok($obj->as_string, "<boolean>$val</boolean>");
    ok($obj->type, 'boolean');
}
# This should not
$obj = new RPC::XML::boolean 'of course!';
ok(! ref $obj);
ok($RPC::XML::ERROR =~ /::new: Value must be one of/);

# The dateTime.iso8601 type shares all code save for type() with the above, so
# only test that one here
$obj = new RPC::XML::datetime_iso8601 time2iso8601(time);
ok($obj->type, 'dateTime.iso8601');
ok(length($obj->as_string), $obj->length);

# Test the base64 type
require MIME::Base64;
$val = MIME::Base64::encode_base64(q/one reasonable-length string/, '');
$obj = new RPC::XML::base64(q/one reasonable-length string/);
ok(ref $obj);
ok($obj->as_string, "<base64>$val</base64>");
# test length()
ok(length($obj->as_string), $obj->length);
$obj = new RPC::XML::base64 $val, 'pre-encoded';
ok(ref $obj);
ok($obj->value, q/one reasonable-length string/);
$obj = new RPC::XML::base64 ();
ok(! ref($obj));
ok($RPC::XML::ERROR =~ /::new: Must be called with non-null data/);

# Now we throw some junk at smart_encode()
@values = smart_encode(__FILE__, 10, 3.14159, '2112',
                       RPC::XML::string->new('2112'), [], {});

ok($values[0]->type, 'string');
ok($values[1]->type, 'int');
ok($values[2]->type, 'double');
ok($values[3]->type, 'int');    # Should have been encoded int regardless of ''
ok($values[4]->type, 'string'); # Was given an object explicitly
ok($values[5]->type, 'array');
ok($values[6]->type, 'struct');

# Arrays
$obj = new RPC::XML::array 1 .. 10;
ok(ref $obj);
ok($obj->type, 'array');
@values = @{ $obj->value };
ok(@values == 10);
@values = @{ $obj->value(1) };
ok(ref($values[0]) && ($values[0]->type eq 'int'));
ok($obj->as_string =~ m|<array>.*(<int>\d+</int>.*){10}.*</array>|sm);
ok(length($obj->as_string), $obj->length);

# Structs
$obj = new RPC::XML::struct (key1 => 1, key2 => 2);
ok(ref $obj);
ok($obj->type, 'struct');
$val = $obj->value;
ok(ref($val) eq 'HASH');
ok(scalar(keys %$val) == 2);
ok($val->{key1} == 1);
$val = $obj->value(1);
ok(ref($val->{key1}) && ($val->{key1}->type eq 'int'));
$val->{key1} = RPC::XML::string->new('hello');
$obj = new RPC::XML::struct $val;
ok(ref $obj);
ok(($obj->value)->{key1} eq 'hello');
ok(($obj->value(1))->{key1}->type eq 'string');
ok($obj->as_string =~ m|<struct>.*(<member>.*
                                   <name>.*</name>.*
                                   <value>.*</value>.*
                                   </member>.*){2}.*</struct>|smx);
ok(length($obj->as_string), $obj->length);

# Faults are a subclass of structs
$obj = new RPC::XML::fault (faultCode => 1, faultString => 'test');
ok(ref $obj);
# Since it's a subclass, I won't waste cycles testing the similar methods
$obj = new RPC::XML::fault (faultCode => 1, faultString => 'test',
                            faultFail => 'extras are not allowed');
ok(! ref($obj));
ok($RPC::XML::ERROR =~ /:new: Extra struct/);
$obj = new RPC::XML::fault (1, 'test');
ok(ref $obj);
ok($obj->code == 1);
ok($obj->string eq 'test');
ok($obj->as_string =~ m|<fault>.*
                          <value>.*
                            <struct>.*
                              (<member>.*
                               <name>.*</name>.*
                               <value>.*</value>.*
                               </member>.*){2}.*
                            </struct>.*
                          </value>.*
                        </fault>|smx);
ok(length($obj->as_string), $obj->length);

# Requests
$obj = new RPC::XML::request 'test.method';
ok(ref $obj);
ok($obj->name eq 'test.method');
ok($obj->args && (@{ $obj->args } == 0));
$obj = new RPC::XML::request ();
ok(! ref($obj));
ok($RPC::XML::ERROR =~ /:new: At least a method name/);
$obj = new RPC::XML::request 'test.method', (1 .. 10);
ok($obj->args && (@{ $obj->args } == 10));
# The new() method uses smart_encode on the args, which has already been
# tested. These are just to ensure that it *does* in fact call it
ok($obj->args->[0]->type eq 'int');
ok($obj->args->[9]->value == 10);
ok($obj->as_string =~ m|<\?xml.*
                        <methodCall>.*
                          <methodName>.*</methodName>.*
                          <params>.*
                            (<param>.*</param>.*){10}.*
                          </params>.*
                        </methodCall>|smx);
ok(length($obj->as_string), $obj->length);

# Responses
$obj = new RPC::XML::response 'ok';
ok(ref $obj);
ok($obj->value->type eq 'string');
ok($obj->value->value eq 'ok');
ok(! $obj->is_fault);
ok($obj->as_string =~ m|<\?xml.*
                        <methodResponse>.*
                          <params>.*
                            <param>.*</param>.*
                          </params>.*
                        </methodResponse>|smx);
ok(length($obj->as_string), $obj->length);

$obj = new RPC::XML::response ();
ok(! ref($obj));
ok($RPC::XML::ERROR =~ /:new: One of a datatype, value or a fault/);
$obj = new RPC::XML::response (RPC::XML::fault->new(1, 'test'));
ok(ref $obj);
# The other methods have already been tested
ok($obj->is_fault);

exit 0;
