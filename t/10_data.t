#!/usr/bin/perl

## no critic(RequireInterpolationOfMetachars)
## no critic(ProhibitComplexRegexes)

# Test the data-manipulation routines in RPC::XML

use strict;
use warnings;

use Config;
use Module::Load;
use Test::More;
use File::Spec;

use RPC::XML ':all';

my ($val, $str, $obj, $class, %val_tbl, @values, $datetime_avail);
$datetime_avail = eval { load DateTime; 1; };

plan tests => 252;

# First, make sure we can't instantiate any of "abstract" classes directly,
# and also make sure that certain base-class methods properly return when
# (wrongly) called as static methods:
$obj = RPC::XML::simple_type->new('foo');
ok(! ref $obj, 'Attempt to directly construct simple_type failed');
like($RPC::XML::ERROR, qr/Cannot instantiate/, 'Correct error message');
$val = RPC::XML::simple_type->value;
ok(! defined $val, 'Static call to RPC::XML::simple_type::value fails');
like($RPC::XML::ERROR, qr/static method/, 'Correct error message');
ok(! RPC::XML::simple_type->as_string(),
   'Static call to RPC::XML::simple_type::as_string fails');
like($RPC::XML::ERROR, qr/static method/, 'Correct error message');
# RPC::XML::double and RPC::XML::string have their own as_string methods
ok(! RPC::XML::double->as_string(),
   'Static call to RPC::XML::simple_type::as_string fails');
like($RPC::XML::ERROR, qr/static method/, 'Correct error message');
ok(! RPC::XML::string->as_string(),
   'Static call to RPC::XML::simple_type::as_string fails');
like($RPC::XML::ERROR, qr/static method/, 'Correct error message');

# Try instantiating a non-scalar reference
$obj = RPC::XML::int->new([]);
ok(! ref $obj, 'Attempt to instantiate from non-scalar ref failed');
like($RPC::XML::ERROR, qr/not derived from scalar/, 'Correct error message');

# Next, the most basic data-types
%val_tbl = (
    'int'  => int(rand 10_000) + 1,
    i4     => int(rand 10_000) + 1,
    i8     => 2**32,
    double => 0.5,
    string => __FILE__,
);

for (sort keys %val_tbl)
{
    $val = $val_tbl{$_};
    $class = "RPC::XML::$_";
    $obj = $class->new($val);
    isa_ok($obj, $class, "Basic data-type $_");
    is($obj->value, $val, "Basic data-type $_, value check");
    is($obj->as_string, "<$_>$val</$_>",
       "Basic data-type $_, XML serialization");
    is($obj->type, $_, "Basic data-type $_, type identification");
    is(length($obj->as_string), $obj->length,
       "Basic data-type $_, length() method test");
}

# Go again, with each of the values being a blessed scalar reference
my @vals = (1, -1, 2**32, 0.5, __FILE__);
%val_tbl = (
    int    => bless(\(shift @vals), 'Tmp::Scalar::Int'),
    i4     => bless(\(shift @vals), 'Tmp::Scalar::I4'),
    i8     => bless(\(shift @vals), 'Tmp::Scalar::I8'),
    double => bless(\(shift @vals), 'Tmp::Scalar::Double'),
    string => bless(\(shift @vals), 'Tmp::Scalar::String'),
);

for my $type (sort keys %val_tbl)
{
    $val = $val_tbl{$type};
    $class = "RPC::XML::$type";
    $obj = $class->new($val);
    isa_ok($obj, $class, "Data objects from blessed scalar refs, type $type");
    is($obj->value, ${$val},
       "Data objects from blessed scalar refs, type $type, value check");
    is($obj->as_string, "<$type>${$val}</$type>",
       "Data objects from blessed scalar refs, type $type, XML serialization");
    is($obj->type, $type,
       "Data objects from blessed scalar refs, type $type, type ident");
    is(length($obj->as_string), $obj->length,
       "Data objects from blessed scalar refs, type $type, length() method");
}

# A few extra tests for RPC::XML::double to make sure the stringification
# doesn't lead to wonky values:
$obj = RPC::XML::double->new(10.0);
is($obj->as_string, '<double>10.0</double>',
   'RPC::XML::double stringification [1]');
$obj = RPC::XML::double->new(0.50);
is($obj->as_string, '<double>0.5</double>',
   'RPC::XML::double stringification [2]');

# Another little test for RPC::XML::string, to check encoding
$val = 'Subroutine &bogus not defined at <_> line -NaN';
$obj = RPC::XML::string->new($val);
is($obj->value, $val, 'RPC::XML::string extra tests, value check');
is($obj->as_string,
   '<string>Subroutine &amp;bogus not defined at &lt;_&gt; line -NaN</string>',
   'RPC::XML::string extra tests, XML serialization');

# Test for correct handling of encoding a 0 (false but defined)
$val = 0;
$obj = RPC::XML::string->new($val);
is($obj->as_string, '<string>0</string>', q(RPC::XML::string, encoding '0'));

# Type boolean is a little funky

# Each of these should be OK
for my $boolval (qw(0 1 yes no tRuE FaLsE))
{
    $val = ($boolval =~ /0|no|false/i) ? 0 : 1;
    $obj = RPC::XML::boolean->new($boolval);
    isa_ok($obj, 'RPC::XML::boolean', "\$obj($boolval)");
    is($obj->value, $val, "RPC::XML::boolean($boolval), value check");
    is($obj->as_string, "<boolean>$val</boolean>",
       "RPC::XML::boolean($boolval), XML serialization");
    is($obj->type, 'boolean', "RPC::XML::boolean($boolval), type ident");
}
# This should not
$obj = RPC::XML::boolean->new('of course!');
ok(! ref $obj, 'RPC::XML::boolean, bad value did not yield referent');
like($RPC::XML::ERROR, qr/::new: Value must be one of/,
     'RPC::XML::boolean, bad value correctly set $RPC::XML::ERROR');

# The dateTime.iso8601 type
$val = time2iso8601(time);
$obj = RPC::XML::datetime_iso8601->new($val);
isa_ok($obj, 'RPC::XML::datetime_iso8601', '$obj');
is($obj->type, 'dateTime.iso8601',
   'RPC::XML::datetime_iso8601, type identification');
is(length($obj->as_string), $obj->length,
   'RPC::XML::datetime_iso8601, length() method test');
is($obj->value, $val, 'RPC::XML::datetime_iso8601, value() method test');
$obj = RPC::XML::datetime_iso8601->new(\$val);
isa_ok($obj, 'RPC::XML::datetime_iso8601', '$obj');
is($obj->type, 'dateTime.iso8601',
   'RPC::XML::datetime_iso8601, type identification (ref)');
is(length($obj->as_string), $obj->length,
   'RPC::XML::datetime_iso8601, length() method test (ref)');
is($obj->value, $val, 'RPC::XML::datetime_iso8601, value() method test (ref)');
# Add a fractional part and try again
chop $val; # Lose the 'Z'
$val .= '.125Z';
$obj = RPC::XML::datetime_iso8601->new($val);
isa_ok($obj, 'RPC::XML::datetime_iso8601', '$obj');
is($obj->type, 'dateTime.iso8601',
   'RPC::XML::datetime_iso8601, type identification');
is(length($obj->as_string), $obj->length,
   'RPC::XML::datetime_iso8601, length() method test');
is($obj->value, $val, 'RPC::XML::datetime_iso8601, value() method test');
# Test bad date-data
$obj = RPC::XML::datetime_iso8601->new();
ok(! ref $obj,
   'RPC::XML::datetime_iso8601, empty value did not yield referent');
like($RPC::XML::ERROR, qr/::new: Value required/,
     'RPC::XML::datetime_iso8601, empty value correctly set $RPC::XML::ERROR');
$obj = RPC::XML::datetime_iso8601->new('not a date');
ok(! ref $obj,
   'RPC::XML::datetime_iso8601, bad value did not yield referent');
like($RPC::XML::ERROR, qr/::new: Malformed data/,
     'RPC::XML::datetime_iso8601, empty value correctly set $RPC::XML::ERROR');
# Test the slightly different date format
$obj = RPC::XML::datetime_iso8601->new('2008-09-29T12:00:00-07:00');
isa_ok($obj, 'RPC::XML::datetime_iso8601', '$obj');
is($obj->type, 'dateTime.iso8601',
   'RPC::XML::datetime_iso8601, type identification');
is($obj->value, '20080929T12:00:00-07:00',
   'RPC::XML::datetime_iso8601, value() method test');
# Test interoperability with the DateTime package, if it is available
SKIP: {
    if (! $datetime_avail)
    {
        skip 'Module DateTime not available', 4;
    }

    my $dt = DateTime->now();
    (my $dt_str = "$dt") =~ s/-//g;

    $obj = RPC::XML::datetime_iso8601->new("$dt");
    isa_ok($obj, 'RPC::XML::datetime_iso8601', '$obj');
    is($obj->value, $dt_str, 'RPC::XML::datetime_iso8601, from DateTime');

    $obj = smart_encode($dt);
    isa_ok($obj, 'RPC::XML::datetime_iso8601', '$obj');
    is($obj->value, $dt_str,
       'RPC::XML::datetime_iso8601, from DateTime via smart_encode');
}

# Test the base64 type
require MIME::Base64;
$str = 'one reasonable-length string';
$val = MIME::Base64::encode_base64($str, q{});
$obj = RPC::XML::base64->new($str);
isa_ok($obj, 'RPC::XML::base64', '$obj');
is($obj->as_string, "<base64>$val</base64>",
   'RPC::XML::base64, XML serialization');
is($obj->value, $str, 'RPC::XML::base64, correct value()');
is(length($obj->as_string), $obj->length,
   'RPC::XML::base64, length() method test');

# Test pre-encoded data
$obj = RPC::XML::base64->new($val, 'pre-encoded');
isa_ok($obj, 'RPC::XML::base64', '$obj (pre-encoded)');
is($obj->value, $str, 'RPC::XML::base64(pre-encoded), value check');

# Test passing in a reference
$obj = RPC::XML::base64->new(\$str);
isa_ok($obj, 'RPC::XML::base64', '$obj');
is($obj->value, $str, 'RPC::XML::base64, correct value()');

# Test a null Base64 object
$obj = RPC::XML::base64->new();
isa_ok($obj, 'RPC::XML::base64', '$obj');
is($obj->value, q{}, 'Zero-length base64 object value OK');
is($obj->as_string, '<base64></base64>',
   'Zero-length base64 object stringifies OK');

# Now we throw some junk at smart_encode()
@values = smart_encode(
    __FILE__,                      # [0] string
    10,                            # [1] int
    3.14159,                       # [2] double
    '2112',                        # [3] int
    RPC::XML::string->new('2112'), # [4] string
    [],                            # [5] array
    {},                            # [6] struct
    \'foo',                        # [7] string
    \2,                            # [8] int
    \1.414,                        # [9] double
    2_147_483_647,                 # [10] int
    -2_147_483_648,                # [11] int
    9_223_372_036_854_775_807,     # [12] i8
    -9_223_372_036_854_775_808,    # [13] i8
    4_294_967_295,                 # [14] i8
    '2009-09-03T10:25:00',         # [15] dateTime.iso8601
    '20090903T10:25:00Z',          # [16] dateTime.iso8601
    '2009-09-03T10:25:00.125',     # [17] dateTime.iso8601
);

is($values[0]->type, 'string', 'smart_encode, string<1>');
is($values[1]->type, 'int', 'smart_encode, int<1>');
is($values[2]->type, 'double', 'smart_encode, double<1>');
# Should have been encoded int regardless of ''
is($values[3]->type, 'int', 'smart_encode, int<2>');
# Was given an object explicitly
is($values[4]->type, 'string', 'smart_encode, string<2>');
is($values[5]->type, 'array', 'smart_encode, array');
is($values[6]->type, 'struct', 'smart_encode, struct');
is($values[7]->type, 'string', 'smart_encode, string<3>');
is($values[8]->type, 'int', 'smart_encode, int<3>');
is($values[9]->type, 'double', 'smart_encode, double<2>');
is($values[10]->type, 'int', 'smart_encode, int<4>');
is($values[11]->type, 'int', 'smart_encode, int<5>');
SKIP: {
    if ($Config{longsize} != 8)
    {
        skip '64-bit architecture required to test these I8 values', 2;
    }

    is($values[12]->type, 'i8', 'smart_encode, i8<1>');
    is($values[13]->type, 'i8', 'smart_encode, i8<2>');
}
is($values[14]->type, 'i8', 'smart_encode, i8<3>');
is($values[15]->type, 'dateTime.iso8601', 'smart_encode, dateTime.iso8601');
is($values[16]->type, 'dateTime.iso8601', 'smart_encode, dateTime.iso8601<2>');
is($values[17]->type, 'dateTime.iso8601', 'smart_encode, dateTime.iso8601<3>');

# Without $RPC::XML::ALLOW_NIL set, smart_encode should encode this as a null
# string:
$obj = smart_encode(undef);
is($obj->type, 'string', 'smart_encode undef->string type');
is($obj->value, q{}, 'smart_encode undef->string value');

# Check that smart_encode gives up on un-convertable references
{
    my $badvalue;
    my $result = eval { $badvalue = smart_encode(\*STDIN); 1; };
    ok(! ref($badvalue),
       'smart_encode, bad reference argument did not yield referent');
    like($@, qr/Un-convertable reference/,
         'smart_encode, bad reference argument set $@ as expected');
}

# Arrays
$obj = RPC::XML::array->new(1 .. 10);
isa_ok($obj, 'RPC::XML::array', '$obj');
is($obj->type, 'array', 'RPC::XML::array, type identification');
@values = @{$obj->value};
is(scalar(@values), 10, 'RPC::XML::array, array size test');
@values = @{$obj->value(1)};
ok(ref($values[0]) && ($values[0]->type eq 'int'),
   'RPC::XML::array, array content is RPC::XML::* referent');
like($obj->as_string, qr{<array>.*(<int>\d+</int>.*){10}.*</array>}smx,
     'RPC::XML::array, XML serialization');
is(length($obj->as_string), $obj->length,
   'RPC::XML::array, length() method test');

# Blessed array references
my $arrayobj = bless [ 1 .. 10 ], "Tmp::Array$$";
$obj = RPC::XML::array->new(from => $arrayobj);
isa_ok($obj, 'RPC::XML::array', '$obj from blessed arrayref');
is($obj->type, 'array',
   'RPC::XML::array from blessed arrayref, type identification');
@values = @{$obj->value};
is(scalar(@values), 10,
   'RPC::XML::array from blessed arrayref, array size test');
@values = @{$obj->value(1)};
ok(ref($values[0]) && ($values[0]->type eq 'int'),
   'RPC::XML::array from blessed arrayref, array content is referent');
like($obj->as_string, qr{<array>.*(<int>\d+</int>.*){10}.*</array>}smx,
     'RPC::XML::array from blessed arrayref, XML serialization');
is(length($obj->as_string), $obj->length,
   'RPC::XML::array from blessed arrayref, length() method test');
undef $arrayobj;

# Structs
$obj = RPC::XML::struct->new(key1 => 1, key2 => 2);
isa_ok($obj, 'RPC::XML::struct', '$obj');
is($obj->type, 'struct', 'RPC::XML::struct, type identification');
$val = $obj->value;
is(ref($val), 'HASH', 'RPC::XML::struct, ref-type of value()');
is(scalar(keys %{$val}), 2, 'RPC::XML::struct, correct number of keys');
is($val->{key1}, 1, q(RPC::XML::struct, 'key1' value test));
$val = $obj->value(1);
ok(ref($val->{key1}) && ($val->{key1}->type eq 'int'),
   'RPC::XML::struct, key-value is referent in shallow conversion');
$val->{key1} = RPC::XML::string->new('hello');
$obj = RPC::XML::struct->new($val);
isa_ok($obj, 'RPC::XML::struct', '$obj(object-values)');
is(($obj->value)->{key1}, 'hello',
   q{RPC::XML::struct(object-values), 'key1' value test});
is(($obj->value(1))->{key1}->type, 'string',
   'RPC::XML::struct(object-values), value-object type correctness');
like($obj->as_string, qr{<struct>
                         (<member>
                             <name>.*</name>
                             <value>.*</value>
                          </member>){2}
                         </struct>}smx,
     'RPC::XML::struct, XML serialization');
is(length($obj->as_string), $obj->length,
   'RPC::XML::struct, length() method test');
# Test handling of keys that contain XML special characters
$obj = RPC::XML::struct->new(q{>}  => 'these',
                             q{<}  => 'are',
                             q{&}  => 'special',
                             q{<>} => 'XML',
                             q{&&} => 'characters');
isa_ok($obj, 'RPC::XML::struct', '$obj(with XML special char keys)');
is((my $tmp = $obj->as_string) =~ tr/&/&/, 7,
   'RPC::XML::struct, XML-encoding of serialized form with char entities');

# Blessed struct reference
my $structobj = bless { key1 => 1, key2 => 2 }, "Tmp::Struct$$";
$obj = RPC::XML::struct->new($structobj);
isa_ok($obj, 'RPC::XML::struct', '$obj(struct<1>)');
is($obj->type, 'struct', 'struct object type method');
$val = $obj->value;
isa_ok($val, 'HASH', 'struct $obj->value');
is(scalar(keys %{$val}), 2, 'struct obj number of keys test');
is($val->{key1}, 1, 'struct obj "key1" test');
$val = $obj->value(1);
isa_ok($val->{key1}, 'RPC::XML::int', '$val->{key1} (shallow eval)');
$val->{key1} = RPC::XML::string->new('hello');
$obj = RPC::XML::struct->new($val);
isa_ok($obj, 'RPC::XML::struct', '$obj(struct<2>)');
is(($obj->value)->{key1}, 'hello', 'struct<2> "key1" test');
is(($obj->value(1))->{key1}->type, 'string', 'struct<2> "key1" type test');
like($obj->as_string, qr{<struct>
                         (<member>
                             <name>.*</name>
                             <value>.*</value>
                          </member>){2}
                         </struct>}smx,
     'struct<2> XML serialization');
is(length($obj->as_string), $obj->length, 'struct<2> length() check');
# No need to re-test the XML character handling

# Faults are a subclass of structs
$obj = RPC::XML::fault->new(faultCode => 1, faultString => 'test');
isa_ok($obj, 'RPC::XML::fault', '$obj (fault)');
# Since it's a subclass, I won't waste cycles testing the similar methods
$obj = RPC::XML::fault->new(faultCode => 1);
ok(! ref $obj, 'fault class constructor fails on missing key(s)');
like($RPC::XML::ERROR, qr/:new: Missing required struct fields/,
     'fault class failure set error string');
$obj = RPC::XML::fault->new(faultCode => 1, faultString => 'test',
                            faultFail => 'extras are not allowed');
ok(! ref($obj), 'fault class rejects extra args');
like($RPC::XML::ERROR, qr/:new: Extra struct/,
     'fault class failure set error string');
$obj = RPC::XML::fault->new(1, 'test');
isa_ok($obj, 'RPC::XML::fault', '$obj<2> (fault)');
is($obj->code, 1, 'fault code() method');
is($obj->string, 'test', 'fault string() method');
like($obj->as_string, qr{<fault>
                           <value>
                             <struct>
                               (<member>
                                  <name>.*</name>
                                  <value>.*</value>
                                </member>.*){2}
                             </struct>
                           </value>
                         </fault>}smx,
     'fault XML serialization');
is(length($obj->as_string), $obj->length, 'fault length() check');

# Requests
$obj = RPC::XML::request->new('test.method');
isa_ok($obj, 'RPC::XML::request', '$obj (request)');
is($obj->name, 'test.method', 'request name method');
ok($obj->args && (@{$obj->args} == 0), 'request args method');
$obj = RPC::XML::request->new();
ok(! ref($obj), 'bad request contructor failed');
like($RPC::XML::ERROR, qr/:new: At least a method name/,
     'bad request constructor set error string');
$obj = RPC::XML::request->new(q{#*}); # Bad method name, should fail
ok(! ref($obj), 'Bad method name in constructor failed');
like($RPC::XML::ERROR, qr/Invalid method name/,
     'Bad method name in constructor set error string');
$obj = RPC::XML::request->new('test.method', (1 .. 10));
ok($obj->args && (@{ $obj->args } == 10), 'request args method size test');
# The new() method uses smart_encode on the args, which has already been
# tested. These are just to ensure that it *does* in fact call it
is($obj->args->[0]->type, 'int', 'request args elt[0] type test');
is($obj->args->[9]->value, 10, 'request args elt[9] value test');
like($obj->as_string, qr{<[?]xml.*?>
                         <methodCall>
                           <methodName>.*</methodName>
                           <params>
                             (<param>.*</param>){10}
                           </params>
                         </methodCall>}smx,
     'request XML serialization');
is(length($obj->as_string), $obj->length, 'request length() test');

# Responses
$obj = RPC::XML::response->new('ok');
isa_ok($obj, 'RPC::XML::response', '$obj (response)');
is($obj->value->type, 'string', 'response value->type test');
is($obj->value->value, 'ok', 'response value->value test');
ok(! $obj->is_fault, 'response object not fault');
like($obj->as_string, qr{<[?]xml.*?>
                         <methodResponse>
                           <params>
                             <param>.*</param>
                           </params>
                         </methodResponse>}smx,
     'response XML serialization');
is(length($obj->as_string), $obj->length, 'response length() test');

$obj = RPC::XML::response->new();
ok(! ref($obj), 'bad response constructor failed');
like($RPC::XML::ERROR, qr/new: One of a datatype, value or a fault/,
     'bad response constructor set error string');
$obj = RPC::XML::response->new(qw(one two));
ok(! ref($obj), 'bad response constructor failed');
like($RPC::XML::ERROR, qr/only one argument/,
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
        256**4+1    => 'i8',    # will do *-1 as well
        256**8+1    => 'double',
        1e37+1      => 'string',
    );

    while (my ($value, $type) = each %map)
    {
        for my $mod (1,-1)
        {
            {
                $obj = smart_encode($mod * $value);
                ok($obj, "smart_encode zealousness test, $mod * $value");
                is($obj->type, $type,
                   'smart_encode zealousness, non-forced type');
            }

            ### test force string encoding
            {
                ### double assign to silence -w
                local $RPC::XML::FORCE_STRING_ENCODING = 1;
                local $RPC::XML::FORCE_STRING_ENCODING = 1;
                $obj = smart_encode($mod * $value);
                ok($obj,
                   "smart_encode zealousness test, $mod * $value (force)");
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
