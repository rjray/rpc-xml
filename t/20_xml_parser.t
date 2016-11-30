#!/usr/bin/env perl

# Test the RPC::XML::Parser::XMLParser class

## no critic(RequireInterpolationOfMetachars)
## no critic(RequireBriefOpen)
## no critic(RequireCheckedClose)

use strict;
use warnings;

use Carp qw(carp croak);
use Test::More;
use File::Spec;

use RPC::XML ':all';
use RPC::XML::Parser::XMLParser;

my ($p, $req, $res, $ret, $dir, $vol, $file, $fh, $str, $badstr);

plan tests => 137;

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, q{});
$file = File::Spec->catfile($dir, 'svsm_text.gif');

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# RPC::XML::* classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Parser::XMLParser.

$p = RPC::XML::Parser::XMLParser->new();
isa_ok($p, 'RPC::XML::Parser::XMLParser', '$p');
isa_ok($p, 'RPC::XML::Parser', '$p');

# Make sure you can't call parse_more() or parse_done() on a vanilla
# RPC::XML::Parser::XMLParser instance:
$ret = eval { $p->parse_more(); 1; };
ok(! $ret, 'Calling parse_more on $p failed');
like($@, qr/Must be called on a push-parser instance/,
     'Correct error message');
$ret = eval { $p->parse_done(); 1; };
ok(! $ret, 'Calling parse_done on $p failed');
like($@, qr/Must be called on a push-parser instance/,
     'Correct error message');

$req = RPC::XML::request->new('test.method');
$ret = $p->parse($req->as_string);
isa_ok($ret, 'RPC::XML::request', '$ret');
is($ret->name, 'test.method', 'Correct request method name');
# Try a request with no <params> block at all:
$str = <<'EO_STR';
<?xml version="1.0" encoding="us-ascii"?>
<methodCall>
  <methodName>test.method</methodName>
</methodCall>
EO_STR
$ret = $p->parse($str);
isa_ok($ret, 'RPC::XML::request', '$ret');
is($ret->name, 'test.method', 'Correct request method name');
ok(ref($ret->args) eq 'ARRAY' && @{$ret->args} == 0,
   'No <params> block yields correct args list');

$res = RPC::XML::response->new(RPC::XML::string->new('test response'));
$ret = $p->parse($res->as_string);
isa_ok($ret, 'RPC::XML::response', '$ret');
is($ret->value->value, 'test response', 'Response value');

# Test some badly-formed data
my $tmp = $res->as_string; $tmp =~ s/methodResponse/mR/g;
$ret = $p->parse($tmp);
ok(! ref($ret), 'Bad XML did not parse');
like($ret, qr/Unknown tag/, 'Parse failure returned error');

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
    if (ref($ret) ne 'RPC::XML::request') {
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
$p = RPC::XML::Parser::XMLParser->new(base64_to_fh => 1);
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
is($base64->as_string(), $new_base64->as_string,
   'Parse base64 spooling, value comparison');
isa_ok($new_base64->{value_fh}, 'GLOB', '$new_base64->{value_fh}');

# Per problem reported by Bill Moseley, check that messages parsed by the
# parser class handle the core entities.
$tmp = q{Entity test: & < > ' "};
$res = RPC::XML::response->new($tmp);
$ret = $p->parse($res->as_string);
is($ret->value->value, $tmp, 'RPC::XML::Parser handles core entities');

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
$p = RPC::XML::Parser::XMLParser->new();
$ret = $p->parse($bad_entities);
SKIP: {
    if (! ref $ret) {
        skip 'Weird entities parsing error in XML::Parser encountered', 1;
    }

    my $args = $ret->args;
    is($args->[0]->value, 'Entity test: ', 'Bad entities ignored');
}

# Now test passing of various references to the parser
$p = RPC::XML::Parser::XMLParser->new();
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
like($ret, qr/no element found/, 'Correct error message');
SKIP: {
    if (! open $fh, '+>', $tmpfile)
    {
        skip "Open of $tmpfile failed, cannot test on it ($!)", 2;
    }

    print {$fh} $str;
    seek $fh, 0, 0;

    $ret = $p->parse($fh);
    ok(! ref $ret, '$ret error from glob reference');
    like($ret, qr/no element found/, 'Correct error message');

    close $fh;
    unlink $tmpfile;
}
# Try an unusable reference
$ret = $p->parse([]);
ok(! ref $ret, 'Unusable reference did not parse to anything');
like($ret, qr/Unusable reference type/, 'Correct error message');

# Negative testing-- try to break the parser
my $bad_counter = 1;
sub test_bad_xml
{
    my ($badstring, $message) = @_;

    $ret = $p->parse($badstring);
    ok(! ref $ret, "Bad XML <$bad_counter>");
    like($ret, qr/$message/, 'Correct error message');

    $bad_counter++;

    return;
}

$str = RPC::XML::request->new('name', 'foo')->as_string;
($badstr = $str) =~ s/>name</>bad^name</;
test_bad_xml($badstr, 'Invalid method name specified');
($badstr = $str) =~ s{<methodName>.*</methodName>}{};
test_bad_xml($badstr, 'No methodName tag detected');
($badstr = $str) =~ s{<params>}{<params></params><params>};
test_bad_xml($badstr, 'Extra content in "methodCall"');
($badstr = $str) =~ s{params>}{paramss>}g;
test_bad_xml($badstr, 'Unknown tag encountered: paramss');

$str = RPC::XML::response->new(1)->as_string;
($badstr = $str) =~ s{<params>}{<params></params><params>};
test_bad_xml($badstr, 'Stack corruption detected');
($badstr = $str) =~ s{<param>}{<param></param><param>};
test_bad_xml($badstr, 'No <value> found within <param> container');
($badstr = $str) =~ s{param>}{paramm>}g;
test_bad_xml($badstr, 'Unknown tag encountered: paramm');
($badstr = $str) =~ s{<value>}{<value></value><value>};
test_bad_xml($badstr, 'Illegal content in param tag');
($badstr = $str) =~ s{value>}{valuee>}g;
test_bad_xml($badstr, 'Unknown tag encountered: valuee');
($badstr = $str) =~ s{>1<}{>foo<};
test_bad_xml($badstr, 'Bad integer');
($badstr = $str) =~ s{params}{paramss}g;
test_bad_xml($badstr, 'Unknown tag encountered: paramss');

$str = RPC::XML::response->new(RPC::XML::fault->new(1, 'foo'))->as_string;
($badstr = $str) =~ s{<fault>}{<fault><value></value>};
test_bad_xml($badstr, 'Stack corruption detected');
($badstr = $str) =~ s{<fault><value>}{<fault><valuee>};
$badstr =~ s{</value></fault>}{</valuee></fault>};
test_bad_xml($badstr, 'Unknown tag encountered: valuee');

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
test_bad_xml($badstr, 'Bad content inside struct block');
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
test_bad_xml($badstr, 'Extra struct fields not allowed');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <fault></fault>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Stack corruption detected');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <fault>
    <value><string>foo</string></value>
  </fault>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Only a <struct> value may be within a <fault>');

$RPC::XML::ALLOW_NIL = 1;
$str = RPC::XML::response->new(undef)->as_string;
($badstr = $str) =~ s{<nil/>}{<nil>undef</nil>};
test_bad_xml($badstr, '<nil /> element must be empty');

$str = RPC::XML::request->new('foo', 1)->as_string;
($badstr = $str) =~ s{<params>}{<params><value></value>};
test_bad_xml($badstr, 'Illegal content in params tag');
($badstr = $str) =~ s{<params>.*</params>}{<params><value></value></params>};
test_bad_xml($badstr, 'Illegal content in params tag');
($badstr = $str) =~ s{<param><value>}{<param><valuee>};
$badstr =~ s{</value></param>}{</valuee></param>};
test_bad_xml($badstr, 'Unknown tag encountered: valuee');
($badstr = $str) =~ s{<value>}{<value><int>1</int>};
test_bad_xml($badstr, 'Stack corruption detected');
($badstr = $str) =~ s{<int>1</int>}{<double>foo</double>};
test_bad_xml($badstr, 'Bad floating-point data read');

# Parser errors specific to arrays:
$str = RPC::XML::response->new([ 1 ])->as_string;
($badstr = $str) =~ s{<array>}{<array><value></value>};
test_bad_xml($badstr, 'Illegal content in array tag');
($badstr = $str) =~ s{<data><value>}{<data><valuee>};
$badstr =~ s{</value></data>}{</valuee></data>};
test_bad_xml($badstr, 'Unknown tag encountered: valuee');
($badstr = $str) =~ s{<int>1</int>}{<int>foo</int>};
test_bad_xml($badstr, 'Bad integer data read');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
    <param>
      <value>
        <array>
          <data>
            <value><int>1</int></value>
            <name>foo</name>
          </data>
        </array>
      </value>
    </param>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Bad content inside data block');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
    <param>
      <value>
        <array>
          <data>
            <name>foo</name>
            <value><int>1</int></value>
          </data>
        </array>
      </value>
    </param>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Illegal content in data tag');

# Parser errors specific to structs:
$str = RPC::XML::response->new({ foo => 1 })->as_string;
($badstr = $str) =~ s{<member>}{<member><foo />};
test_bad_xml($badstr, 'Unknown tag encountered: foo');
($badstr = $str) =~ s{name>}{namee>}g;
test_bad_xml($badstr, 'Unknown tag encountered: namee');
($badstr = $str) =~ s{<int>1</int>}{<int>foo</int>};
test_bad_xml($badstr, 'Bad integer data');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
    <param>
      <value>
        <struct>
          <member>
            <name>foo</name>
            <value><int>1</int></value>
            <value><int>1</int></value>
          </member>
        </struct>
      </value>
    </param>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Element mismatch, expected to see name');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
    <param>
      <value>
        <struct>
          <member>
            <value><int>1</int></value>
            <name>foo</name>
          </member>
        </struct>
      </value>
    </param>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Element mismatch, expected to see value');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
    <param>
      <value>
        <struct>
          <member>
            <name>foo</name>
            <value><int>1</int></value>
          </member>
          <value><int>1</int></value>
        </struct>
      </value>
    </param>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Element mismatch, expected to see member');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
    <param>
      <value>
        <struct>
          <value><int>1</int></value>
          <member>
            <name>foo</name>
            <value><int>1</int></value>
          </member>
        </struct>
      </value>
    </param>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'Bad content inside struct block');

# Some corner-cases in responses
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
    <param>
      <value><int>1</int></value>
    </param>
    <param>
      <value><int>1</int></value>
    </param>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'invalid: too many params');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
  <params>
  </params>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'invalid: no params');
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodResponse>
</methodResponse>
EO_BADSTR
test_bad_xml($badstr, 'No parameter was declared');

# Corner case(s) in requests
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<methodCall>
  <name>foo</name>
  <methodName>foo</methodName>
  <params></params>
</methodCall>
EO_BADSTR
test_bad_xml($badstr, 'methodName tag must immediately follow a methodCall');

# Test the "none of the above" error case
($badstr = $str) =~ s/struct/structt/g;
test_bad_xml($badstr, 'Unknown tag encountered: structt');

# Test parse-end errors
$badstr = <<'EO_BADSTR';
<?xml version="1.0" encoding="us-ascii"?>
<params>
  <param>
    <value><int>1</int></value>
  </param>
</params>
EO_BADSTR
test_bad_xml($badstr, 'End-of-parse error');

# Test some of the failures related to Base64-spooling. This can only be tested
# on non-Windows systems, as to cause some of the failures we'll need to create
# an un-writable directory (and Windows doesn't have the same chmod concept we
# have in other places).
SKIP: {
    if ($^O eq 'MSWin32' || $^O eq 'cygwin')
    {
        skip 'Tests involving directory permissions skipped on Windows', 1;
    }
    # Also cannot be reliably tested if running as root:
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

    $p = RPC::XML::Parser::XMLParser->new(
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
