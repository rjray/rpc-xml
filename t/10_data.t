#!/usr/bin/perl
# $Id$

# Test the data-manipulation routines in RPC::XML

use strict;
use vars qw($val $obj $class %val_tbl @values);

use Test::More tests => 178;
use RPC::XML ':all';

# First, the most basic data-types
%val_tbl = (
            'int'  => int(rand 10000) + 1,
            i4     => int(rand 10000) + 1,
            double => 0.5,
            string => __FILE__
           );

for (sort keys %val_tbl)
{
    $val = $val_tbl{$_};
    $class = "RPC::XML::$_";
    $obj = $class->new($val);
    ok(ref $obj, "Basic data-type $_, object is referent");
    is($obj->value, $val, "Basic data-type $_, value check");
    is($obj->as_string, "<$_>$val</$_>",
       "Basic data-type $_, XML serialization");
    is($obj->type, $_, "Basic data-type $_, type identification");
    is(length($obj->as_string), $obj->length,
       "Basic data-type $_, length() method test");
}

# Go again, with each of the values being a blessed scalar reference
my @vals = (int(rand 10000) + 1, int(rand 10000) + 1, 0.5, __FILE__);
%val_tbl = (
            'int'  => bless(\(shift(@vals)), "Tmp::Scalar::Int"),
            i4     => bless(\(shift(@vals)), "Tmp::Scalar::I4"),
            double => bless(\(shift(@vals)), "Tmp::Scalar::Double"),
            string => bless(\(shift(@vals)), "Tmp::Scalar::String")
           );

for (sort keys %val_tbl)
{
    $val = $val_tbl{$_};
    $class = "RPC::XML::$_";
    $obj = $class->new($val);
    ok(ref $obj,
       "Data objects from blessed scalar refs, type $_, object is referent");
    is($obj->value, $$val,
       "Data objects from blessed scalar refs, type $_, value check");
    is($obj->as_string, "<$_>$$val</$_>",
       "Data objects from blessed scalar refs, type $_, XML serialization");
    is($obj->type, $_,
       "Data objects from blessed scalar refs, type $_, type identification");
    is(length($obj->as_string), $obj->length,
       "Data objects from blessed scalar refs, type $_, length() method test");
}

# Another little test for RPC::XML::string, to check encoding
$val = 'Subroutine &bogus not defined at <_> line -NaN';
$obj = RPC::XML::string->new($val);
is($obj->value, $val, "RPC::XML::string extra tests, value check");
is($obj->as_string,
   "<string>Subroutine &amp;bogus not defined at &lt;_&gt; line -NaN</string>",
   "RPC::XML::string extra tests, XML serialization");

# Test for correct handling of encoding a 0 (false but defined)
$val = 0;
$obj = RPC::XML::string->new($val);
is($obj->as_string, "<string>0</string>", "RPC::XML::string, encoding '0'");

# Type boolean is a little funky

# Each of these should be OK
for (qw(0 1 yes no tRuE FaLsE))
{
    $val = (/0|no|false/i) ? 0 : 1;
    $obj = RPC::XML::boolean->new($_);
    ok(ref $obj, "RPC::XML::boolean($_), object is referent");
    is($obj->value, $val, "RPC::XML::boolean($_), value check");
    is($obj->as_string, "<boolean>$val</boolean>",
       "RPC::XML::boolean($_), XML serialization");
    is($obj->type, 'boolean', "RPC::XML::boolean($_), type identification");
}
# This should not
$obj = RPC::XML::boolean->new('of course!');
ok(! ref $obj, "RPC::XML::boolean, bad value did not yield referent");
like($RPC::XML::ERROR, qr/::new: Value must be one of/,
   "RPC::XML::boolean, bad value correctly set \$RPC::XML::ERROR");

# The dateTime.iso8601 type shares all code save for type() with the above, so
# only test that one here
$obj = RPC::XML::datetime_iso8601->new(time2iso8601(time));
is($obj->type, 'dateTime.iso8601',
   "RPC::XML::datetime_iso8601, type identification");
is(length($obj->as_string), $obj->length,
   "RPC::XML::datetime_iso8601, length() method test");

# Test the base64 type
require MIME::Base64;
$val = MIME::Base64::encode_base64(q/one reasonable-length string/, '');
$obj = RPC::XML::base64->new(q/one reasonable-length string/);
ok(ref $obj, "RPC::XML::base64, object is referent");
is($obj->as_string, "<base64>$val</base64>",
   "RPC::XML::base64, XML serialization");
# test length()
is(length($obj->as_string), $obj->length,
   "RPC::XML::base64, length() method test");
$obj = RPC::XML::base64->new($val, 'pre-encoded');
ok(ref $obj, "RPC::XML::base64(pre-encoded), object is referent");
is($obj->value, q/one reasonable-length string/,
   "RPC::XML::base64(pre-encoded), value check");
$obj = RPC::XML::base64->new();
ok(! ref($obj), "RPC::XML::base64(no data), bad value did not yield referent");
like($RPC::XML::ERROR, qr/::new: Must be called with non-null data/,
     "RPC::XML::base64(no data), bad value correctly set \$RPC::XML::ERROR");

# Now we throw some junk at smart_encode()
@values = smart_encode(__FILE__, 10, 3.14159, '2112',
                       RPC::XML::string->new('2112'), [], {}, \"foo", \2,
                       \1.414, );

is($values[0]->type, 'string', "smart_encode, string<1>");
is($values[1]->type, 'int', "smart_encode, int<1>");
is($values[2]->type, 'double', "smart_encode, double<1>");
# Should have been encoded int regardless of ''
is($values[3]->type, 'int', "smart_encode, int<2>");
# Was given an object explicitly
is($values[4]->type, 'string', "smart_encode, string<2>");
is($values[5]->type, 'array', "smart_encode, array");
is($values[6]->type, 'struct', "smart_encode, struct");
is($values[7]->type, 'string', "smart_encode, string<3>");
is($values[8]->type, 'int', "smart_encode, int<3>");
is($values[9]->type, 'double', "smart_encode, double<2>");

# Check that smart_encode gives up on un-convertable references
{
    my $badvalue;
    eval { $badvalue = smart_encode(\*STDIN); };
    ok(! ref($badvalue),
       "smart_encode, bad reference argument did not yield referent");
    like($@, qr/Un-convertable reference/,
         "smart_encode, bad reference argument set \$@ as expected");
}

# Arrays
$obj = RPC::XML::array->new(1 .. 10);
ok(ref $obj, "RPC::XML::array, object is referent");
is($obj->type, 'array', "RPC::XML::array, type identification");
@values = @{ $obj->value };
is(scalar(@values), 10, "RPC::XML::array, array size test");
@values = @{ $obj->value(1) };
ok(ref($values[0]) && ($values[0]->type eq 'int'),
   "RPC::XML::array, array content is RPC::XML::* referent");
like($obj->as_string, qr|<array>.*(<int>\d+</int>.*){10}.*</array>|sm,
     "RPC::XML::array, XML serialization");
is(length($obj->as_string), $obj->length,
   "RPC::XML::array, length() method test");

# Blessed array references
my $arrayobj = bless [ 1 .. 10 ], "Tmp::Array$$";
$obj = RPC::XML::array->new($arrayobj);
ok(ref $obj, "RPC::XML::array from blessed arrayref, object is referent");
is($obj->type, 'array',
   "RPC::XML::array from blessed arrayref, type identification");
@values = @{ $obj->value };
is(scalar(@values), 10,
   "RPC::XML::array from blessed arrayref, array size test");
@values = @{ $obj->value(1) };
ok(ref($values[0]) && ($values[0]->type eq 'int'),
   "RPC::XML::array from blessed arrayref, array content is referent");
like($obj->as_string, qr|<array>.*(<int>\d+</int>.*){10}.*</array>|sm,
     "RPC::XML::array from blessed arrayref, XML serialization");
is(length($obj->as_string), $obj->length,
   "RPC::XML::array from blessed arrayref, length() method test");
undef $arrayobj;

# Structs
$obj = RPC::XML::struct->new(key1 => 1, key2 => 2);
ok(ref $obj, "RPC::XML::struct, object is referent");
is($obj->type, 'struct', "RPC::XML::struct, type identification");
$val = $obj->value;
is(ref($val), 'HASH', "RPC::XML::struct, ref-type of value()");
is(scalar(keys %$val), 2, "RPC::XML::struct, correct number of keys");
is($val->{key1}, 1, "RPC::XML::struct, 'key1' value test");
$val = $obj->value(1);
ok(ref($val->{key1}) && ($val->{key1}->type eq 'int'),
   "RPC::XML::struct, key-value is referent in shallow conversion");
$val->{key1} = RPC::XML::string->new('hello');
$obj = RPC::XML::struct->new($val);
ok(ref $obj,
   "RPC::XML::struct(object-values), object is referent");
is(($obj->value)->{key1}, 'hello',
   "RPC::XML::struct(object-values), 'key1' value test");
is(($obj->value(1))->{key1}->type, 'string',
   "RPC::XML::struct(object-values), value-object type correctness");
like($obj->as_string, qr|<struct>.*(<member>.*
                                      <name>.*</name>.*
                                      <value>.*</value>.*
                                    </member>.*){2}.*</struct>|smx,
     "RPC::XML::struct, XML serialization");
is(length($obj->as_string), $obj->length,
   "RPC::XML::struct, length() method test");
# Test handling of keys that contain XML special characters
$obj = RPC::XML::struct->new('>'  => these   =>
                             '<'  => are     =>
                             '&'  => special =>
                             '<>' => XML     =>
                             '&&' => 'characters');
is((my $tmp = $obj->as_string) =~ tr/&/&/, 7,
   "RPC::XML::struct, XML-encoding of serialized form with char entities");

# Blessed struct reference
my $structobj = bless { key1 => 1, key2 => 2 }, "Tmp::Struct$$";
$obj = RPC::XML::struct->new($structobj);
ok(ref $obj, 'struct<1> object creation');
is($obj->type, 'struct', 'struct object type method');
$val = $obj->value;
isa_ok($val, 'HASH', 'struct $obj->value');
is(scalar(keys %$val), 2, 'struct obj number of keys test');
is($val->{key1}, 1, 'struct obj "key1" test');
$val = $obj->value(1);
isa_ok($val->{key1}, 'RPC::XML::int', '$val->{key1} (shallow eval)');
$val->{key1} = RPC::XML::string->new('hello');
$obj = RPC::XML::struct->new($val);
ok(ref $obj, 'struct<2> object creation');
is(($obj->value)->{key1}, 'hello', 'struct<2> "key1" test');
is(($obj->value(1))->{key1}->type, 'string', 'struct<2> "key1" type test');
like($obj->as_string, qr|<struct>.*(<member>.*
                                      <name>.*</name>.*
                                      <value>.*</value>.*
                                    </member>.*){2}.*</struct>|smx,
     'struct<2> XML serialization');
is(length($obj->as_string), $obj->length, 'struct<2> length() check');
# No need to re-test the XML character handling

# Faults are a subclass of structs
$obj = RPC::XML::fault->new(faultCode => 1, faultString => 'test');
isa_ok($obj, 'RPC::XML::fault', '$obj (fault)');
# Since it's a subclass, I won't waste cycles testing the similar methods
$obj = RPC::XML::fault->new(faultCode => 1, faultString => 'test',
                            faultFail => 'extras are not allowed');
ok(! ref($obj), 'fault class rejects extra args');
like($RPC::XML::ERROR, qr/:new: Extra struct/,
     'fault class failure set error string');
$obj = RPC::XML::fault->new(1, 'test');
isa_ok($obj, 'RPC::XML::fault', '$obj<2> (fault)');
is($obj->code, 1, 'fault code() method');
is($obj->string, 'test', 'fault string() method');
like($obj->as_string, qr|<fault>.*
                           <value>.*
                             <struct>.*
                               (<member>.*
                                  <name>.*</name>.*
                                  <value>.*</value>.*
                                </member>.*){2}.*
                             </struct>.*
                           </value>.*
                         </fault>|smx,
     'fault XML serialization');
is(length($obj->as_string), $obj->length, 'fault length() check');

# Requests
$obj = RPC::XML::request->new('test.method');
isa_ok($obj, 'RPC::XML::request', '$obj (request)');
is($obj->name, 'test.method', 'request name method');
ok($obj->args && (@{ $obj->args } == 0), 'request args method');
$obj = RPC::XML::request->new();
ok(! ref($obj), 'bad request contructor failed');
like($RPC::XML::ERROR, qr/:new: At least a method name/,
     'bad request constructor set error string');
$obj = RPC::XML::request->new('test.method', (1 .. 10));
ok($obj->args && (@{ $obj->args } == 10), 'request args method size test');
# The new() method uses smart_encode on the args, which has already been
# tested. These are just to ensure that it *does* in fact call it
is($obj->args->[0]->type, 'int', 'request args elt[0] type test');
is($obj->args->[9]->value, 10, 'request args elt[9] value test');
like($obj->as_string, qr|<\?xml.*
                         <methodCall>.*
                           <methodName>.*</methodName>.*
                           <params>.*
                             (<param>.*</param>.*){10}.*
                           </params>.*
                         </methodCall>|smx,
     'request XML serialization');
is(length($obj->as_string), $obj->length, 'request length() test');

# Responses
$obj = RPC::XML::response->new('ok');
isa_ok($obj, 'RPC::XML::response', '$obj (response)');
is($obj->value->type, 'string', 'response value->type test');
is($obj->value->value, 'ok', 'response value->value test');
ok(! $obj->is_fault, 'response object not fault');
like($obj->as_string, qr|<\?xml.*
                         <methodResponse>.*
                           <params>.*
                             <param>.*</param>.*
                           </params>.*
                         </methodResponse>|smx,
     'response XML serialization');
is(length($obj->as_string), $obj->length, 'response length() test');

$obj = RPC::XML::response->new();
ok(! ref($obj), 'bad response constructor failed');
like($RPC::XML::ERROR, qr/:new: One of a datatype, value or a fault/,
     'bad response constructor set error string');
$obj = RPC::XML::response->new(RPC::XML::fault->new(1, 'test'));
isa_ok($obj, 'RPC::XML::response', '$obj (response/fault)');
# The other methods have already been tested
ok($obj->is_fault, 'fault response creation is_fault test');

### test for bug where encoding was done too freely, encoding
### any ^\d+$ as int, etc
{
    my %map = (
        256         => 'int',
        256**4+1    => 'double',    # will do *-1 as well
        1e37+1      => 'string',
    );

    while (my($val,$type) = each %map)
    {
        for my $mod (1,-1)
        {
            {
                my $obj = smart_encode($mod * $val);
                ok($obj, "smart_encode zealousness test, $mod * $val");
                is($obj->type, $type,
                   'smart_encode zealousness, non-forced type');
            }

            ### test force string encoding
            {
                ### double assign to silence -w
                local $RPC::XML::FORCE_STRING_ENCODING = 1;
                local $RPC::XML::FORCE_STRING_ENCODING = 1;
                my $obj = smart_encode($mod * $val);
                ok($obj, "smart_encode zealousness test, $mod * $val (force)");
                is($obj->type, 'string',
                   'smart_encode zealousness, forced to string');
            }
        }
    }
}

# Test for RT# 31818, ensure that very small double values are expressed in
# a format that conforms to the XML-RPC spec.
is(RPC::XML::double->new(0.000005)->as_string, '<double>0.000005</double>',
   'Floating-point format test, RT31818');

exit 0;
