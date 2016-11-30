#!/usr/bin/env perl

# Test the RPC::XML::Parser::XMLLibXML class

## no critic(RequireInterpolationOfMetachars)
## no critic(RequireBriefOpen)
## no critic(RequireCheckedClose)

use strict;
use warnings;

use Carp qw(carp croak);
use Module::Load;
use Test::More;
use File::Spec;

use RPC::XML ':all';

my ($p, $req, $res, $str, $badstr, $ret, $dir, $vol, $file, $fh);

if (! eval { load XML::LibXML; 1; })
{
    plan skip_all => 'XML::LibXML not installed';
}
else
{
    load RPC::XML::Parser::XMLLibXML;
    plan tests => 110;
}

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, q{});
$file = File::Spec->catfile($dir, 'svsm_text.gif');

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# RPC::XML::* classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Parser::XMLLibXML.

$p = RPC::XML::Parser::XMLLibXML->new();
isa_ok($p, 'RPC::XML::Parser::XMLLibXML', '$p');
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

# Test parsing of faults
$res = RPC::XML::response->new(RPC::XML::fault->new(1, 'foo'));
$ret = $p->parse($res->as_string);
isa_ok($ret, 'RPC::XML::response', 'fault parsing: $ret');
isa_ok($ret->value, 'RPC::XML::fault', 'fault parsing: $ret->value');
is($ret->value->code, 1, 'fault parsing: correct code value');
is($ret->value->string, 'foo', 'fault parsing: correct string value');

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
    if (ref($ret) ne 'RPC::XML::request')
    {
        skip 'RPC::XML::request object not properly parsed, cannot test.', 20;
    }

    is($ret->name, 'parserTest', 'Properly parsed /methodCall/methodName');
    my $args = $ret->args;
    is(scalar @{$args}, 9, 'Parser created correct-length args list');
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
    is($args->[6]->value, '20080929T12:00:00-07:00',
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
$p = RPC::XML::Parser::XMLLibXML->new(base64_to_fh => 1);
if (! open $fh, '<', $file)
{
    croak "Error opening $file: $!";
}
my $base64 = RPC::XML::base64->new($fh);
$req = RPC::XML::request->new('method', $base64);

# Start testing
my $spool_ret = $p->parse($req->as_string);
isa_ok($spool_ret, 'RPC::XML::request', '$spool_ret');
is($spool_ret->name, 'method', 'Request, base64 spooling, method name test');
ok(ref($spool_ret->args), 'Request, base64 spooling, return arg test');

my $new_base64 = $spool_ret->args->[0];
isa_ok($new_base64, 'RPC::XML::base64', '$new_base64');
is($new_base64->as_string, $base64->as_string(),
   'Parse base64 spooling, value comparison');
isa_ok($new_base64->{value_fh}, 'GLOB', '$new_base64->{value_fh}');

# Per problem reported by Bill Moseley, check that messages parsed by the
# parser class handle the core entities.
$tmp = q{Entity test: & < > ' "};
$res = RPC::XML::response->new($tmp);
$ret = $p->parse($res->as_string);
is($ret->value->value, $tmp, 'RPC::XML::Parser handles core entities');

# The variables $req and $base64 are still in scope, and should still be OK.
# In fact, I should be testing this functionality in the XML::Parser suite as
# well, but the server tests exercise it for that parser.

# Test the push-parser functionality.
my $pp = RPC::XML::Parser::XMLLibXML->new->parse();
isa_ok($pp, 'RPC::XML::Parser::XMLLibXML', 'Push-parser instance');
my $string = $req->as_string;
my $string1 = substr $string, 0, int(length($string)/2);
my $string2 = substr $string, int(length($string)/2);
$pp->parse_more($string1);
$pp->parse_more($string2);
$res = $pp->parse_done();
isa_ok($res, 'RPC::XML::request', 'parse_done() return value');
my $new_b64 = $res->args->[0];
isa_ok($new_b64, 'RPC::XML::base64', 'First args value');
is($new_b64->as_string, $base64->as_string(),
   'Push-parse value comparison');

SKIP: {
    if ($^O eq 'MSWin32')
    {
        skip '/etc/passwd is not present on windows.', 1;
    }

    my $bad_entities = <<'EOX';
<?xml version="1.0" encoding="us-ascii"?>
<!DOCTYPE foo [
    <!ENTITY foo SYSTEM "file:///etc/passwd">
]>
<methodCall>
  <methodName>metaWeblog.newPost</methodName>
  <params>
    <param>
      <value><string>Entity test: &foo;</string></value>
    </param>
  </params>
</methodCall>
EOX
    $pp = RPC::XML::Parser::XMLLibXML->new->parse();
    $ret = $pp->parse($bad_entities);
    my $args = $ret->args;
    is($args->[0]->value, 'Entity test: ', 'Bad entities ignored');
}

# Now test passing of various references to the parser
$p = RPC::XML::Parser::XMLLibXML->new();
$str = RPC::XML::request->new('test.method')->as_string;
$ret = $p->parse(\$str);
isa_ok($ret, 'RPC::XML::request', '$ret from scalar reference');
ok(ref($ret) && ($ret->name eq 'test.method'), 'Correct request method name');
my $tmpfile = File::Spec->catfile($dir, "tmp_$$.xml");
SKIP: {
    if (! open $fh, '+>', $tmpfile)
    {
        skip "Open of $tmpfile failed, cannot test on it ($!)", 2;
    }

    print {$fh} $str;
    seek $fh, 0, 0;

    $ret = $p->parse($fh);
    isa_ok($ret, 'RPC::XML::request', '$ret from glob reference');
    ok((ref $ret and ($ret->name eq 'test.method')),
       'Correct request method name');

    close $fh;
    unlink $tmpfile;
}
# Tweak the XML to test the error cases
$str =~ s{</methodCall>}{};
$ret = $p->parse(\$str);
ok(! ref $ret, '$ret error from scalar reference');
like($ret, qr/parser error/, 'Correct error message');
SKIP: {
    if (! open $fh, '+>', $tmpfile)
    {
        skip "Open of $tmpfile failed, cannot test on it ($!)", 2;
    }

    print {$fh} $str;
    seek $fh, 0, 0;

    $ret = $p->parse($fh);
    ok(! ref $ret, '$ret error from glob reference');
    like($ret, qr/parser error/, 'Correct error message');

    close $fh;
    unlink $tmpfile;
}
# Try an unusable reference
$ret = $p->parse([]);
ok(! ref $ret, 'Unusable reference did not parse to anything');
like($ret, qr/Unusable reference type/, 'Correct error message');

# Negative testing-- try to break the parser
$str = RPC::XML::request->new('name', 'foo')->as_string;
($badstr = $str) =~ s/>name</>bad^name</;
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <1>');
like($ret, qr/methodName value.*not a valid name/, 'Correct error message');
($badstr = $str) =~ s{<methodName>.*</methodName>}{};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <2>');
like($ret, qr/missing "methodName" child-element/, 'Correct error message');
($badstr = $str) =~ s{<params>}{<params></params><params>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <3>');
like($ret, qr/Extra content in "methodCall"/, 'Correct error message');
($badstr = $str) =~ s{params>}{paramss>}g;
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <4>');
like($ret, qr/Unknown tag "paramss"/, 'Correct error message');

$str = RPC::XML::response->new(1)->as_string;
($badstr = $str) =~ s{<params>}{<params></params><params>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <5>');
like($ret, qr/too many child elements/, 'Correct error message');
($badstr = $str) =~ s{<param>}{<param></param><param>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <6>');
like($ret, qr/too many child elements/, 'Correct error message');
($badstr = $str) =~ s{param>}{paramm>}g;
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <7>');
like($ret, qr/Unknown tag "paramm"/, 'Correct error message');
($badstr = $str) =~ s{<value>}{<value></value><value>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <8>');
like($ret, qr/too many child elements/, 'Correct error message');
($badstr = $str) =~ s{value>}{valuee>}g;
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <9>');
like($ret, qr/Unknown tag "valuee"/, 'Correct error message');
($badstr = $str) =~ s{>1<}{>foo<};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <10>');
like($ret, qr/Bad integer/, 'Correct error message');
($badstr = $str) =~ s{params}{paramss}g;
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <11>');
like($ret, qr/Illegal tag "paramss"/, 'Correct error message');

$str = RPC::XML::response->new(RPC::XML::fault->new(1, 'foo'))->as_string;
($badstr = $str) =~ s{<fault>}{<fault><value></value>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <12>');
like($ret, qr/too many child elements/, 'Correct error message');
($badstr = $str) =~ s{<fault><value>}{<fault><valuee>};
$badstr =~ s{</value></fault>}{</valuee></fault>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <13>');
like($ret, qr/Unknown tag "valuee"/, 'Correct error message');
# These are a little more hairy, trying to pass an invalid fault structure.
# Gonna hard-code the strings rather than trying to transform $str.
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <fault>
    <value>
      <struct>
        <value>str</value>
        <member>
          <name>faultString</name>
          <value><string>foo</string></value>
        </member>
        <member>
          <name>faultCode</name>
          <value><int>1</int></value>
        </member>
      </struct>
    </value>
  </fault>
</methodResponse>
EO_BADSTR
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <14>');
like($ret, qr/Bad tag within struct/, 'Correct error message');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <fault>
    <value>
      <struct>
        <member>
          <name>faultString</name>
          <value><string>foo</string></value>
        </member>
        <member>
          <name>faultCode</name>
          <value><int>1</int></value>
        </member>
        <member>
          <name>extraMember</name>
          <value><int>1</int></value>
        </member>
      </struct>
    </value>
  </fault>
</methodResponse>
EO_BADSTR
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <15>');
like($ret, qr/Extra struct fields not allowed/, 'Correct error message');

$RPC::XML::ALLOW_NIL = 1;
$str = RPC::XML::response->new(undef)->as_string;
($badstr = $str) =~ s{<nil/>}{<nil>undef</nil>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <16>');
like($ret, qr/nil tag must be empty/, 'Correct error message');

$str = RPC::XML::request->new('foo', 1)->as_string;
($badstr = $str) =~ s{<params>}{<params><value></value>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <17>');
like($ret, qr/Unknown tag in params: value/, 'Correct error message');
($badstr = $str) =~ s{<param><value>}{<param><valuee>};
$badstr =~ s{</value></param>}{</valuee></param>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <18>');
like($ret, qr/Unknown tag in param: valuee/, 'Correct error message');
($badstr = $str) =~ s{<value>}{<value><int>1</int>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <19>');
like($ret, qr/Too many child-nodes for value tag/, 'Correct error message');
($badstr = $str) =~ s{<int>1</int>}{<double>foo</double>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <20>');
like($ret, qr/Bad floating-point data read/, 'Correct error message');

# Parser errors specific to arrays:
$str = RPC::XML::response->new([ 1 ])->as_string;
($badstr = $str) =~ s{<array>}{<array><foo></foo>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <21>');
like($ret, qr/array tag must have just one child element/,
     'Correct error message');
($badstr = $str) =~ s{<data><value>}{<data><valuee>};
$badstr =~ s{</value></data>}{</valuee></data>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <22>');
like($ret, qr/Bad tag within array: got "valuee"/, 'Correct error message');
($badstr = $str) =~ s{<int>1</int>}{<int>foo</int>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <23>');
like($ret, qr/Bad integer data read/, 'Correct error message');

# Parser errors specific to structs:
$str = RPC::XML::response->new({ foo => 1 })->as_string;
($badstr = $str) =~ s{<member>}{<member><foo />};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <24>');
like($ret, qr/Wrong number of nodes within struct/, 'Correct error message');
($badstr = $str) =~ s{name>}{namee>}g;
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <25>');
like($ret, qr/expected tags "name" and "value"/, 'Correct error message');
($badstr = $str) =~ s{<int>1</int>}{<int>foo</int>};
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <26>');
like($ret, qr/Bad integer data/, 'Correct error message');

# Test the "none of the above" error case
($badstr = $str) =~ s/struct/structt/g;
$ret = $p->parse($badstr);
ok(! ref $ret, 'Bad XML <27>');
like($ret, qr/Unknown tag "structt"/, 'Correct error message');

# Test some of the failures related to Base64-spooling. This can only be tested
# on non-Windows systems, as to cause some of the failures we'll need to create
# an un-writable directory (and Windows doesn't have the same chmod concept we
# have in other places).
SKIP: {
    if ($^O eq 'MSWin32' || $^O eq 'cygwin')
    {
        skip 'Tests involving directory permissions skipped on Windows', 1;
    }
    # Also cannot be reliably run under root:
    if ($< == 0)
    {
        skip 'Tests involving directory permissions skipped under root', 1;
    }

    my $baddir = File::Spec->catdir(File::Spec->tmpdir(), "baddir_$$");
    if (! mkdir $baddir)
    {
        skip "Skipping, failed to create dir $baddir: $!", 1;
    }
    if (! chmod oct(600), $baddir)
    {
        skip "Skipping, failed to chmod dir $baddir: $!", 1;
    }

    $p = RPC::XML::Parser::XMLLibXML->new(
        base64_to_fh    => 1,
        base64_temp_dir => $baddir
    );
    if (! open $fh, '<', $file)
    {
        croak "Error opening $file: $!";
    }
    my $base64fail = RPC::XML::base64->new($fh);
    $req = RPC::XML::request->new('method', $base64fail);
    $ret = $p->parse($req->as_string);

    like($ret, qr/Error opening temp file for base64/,
         'Opening Base64 spoolfile correctly failed');

    if (! rmdir $baddir)
    {
        carp "Failed to remove temp-dir $baddir: $!";
    }
}

exit 0;
