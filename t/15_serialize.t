#!/usr/bin/env perl

# Test the serialization of XML structures to filehandles

## no critic(RequireBriefOpen)
## no critic(RequireCheckedClose)

use strict;
use warnings;

use Carp qw(croak);
use Test::More;
use File::Spec;
use IO::Handle;

use RPC::XML ':all';

plan tests => 20;

my ($dir, $vol, $fh, $file, $tmpfile, $faux_req, $faux_res, $ofh, $data);

# We'll be using the <nil /> extension here:
$RPC::XML::ALLOW_NIL = 1;

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, q{});
$file = File::Spec->catfile($dir, 'svsm_text.gif');
$tmpfile = File::Spec->catfile($dir, "__tmp__${$}__");

END
{
    # Make sure we don't leave any droppings...
    if (-f $tmpfile)
    {
        unlink $tmpfile;
    }
}

if (! (open $fh, '<', $file))
{
    croak "Could not open $file for reading: $!";
}

$faux_req = RPC::XML::request->new(
    'test',
    RPC_STRING 'string',
    RPC_INT 10,
    RPC_I4 20,
    RPC_I8 4_294_967_296,
    RPC_DOUBLE 0.5,
    RPC_BOOLEAN 1,
    RPC_DATETIME_ISO8601 time2iso8601(),
    [ qw(a b c) ],
    { one => 2 },
    RPC_NIL,
    RPC_BASE64 $fh
);

# This is a good place to test the length() method, while we're at it
is(length($faux_req->as_string), $faux_req->length, 'Testing length() method');

if (! (open $ofh, '+>', $tmpfile))
{
    croak "Could not open $tmpfile for read/write: $!";
}
$ofh->autoflush(1);

$faux_req->serialize($ofh);
ok(1, 'serialize method did not croak'); # Just happy we made it this far.

is(-s $ofh, length($faux_req->as_string), 'File size is correct');

seek $ofh, 0, 0;
$data = q{};
read $ofh, $data, -s $ofh;

is($data, $faux_req->as_string, 'File content is correct');

# Done with these for now
close $fh;
close $ofh;
unlink $tmpfile;

# We'll be doing this next set twice, as RPC::XML::response::serialize has a
# slightly different code-path for faults and all other responses.
if (! (open $ofh, '+>', $tmpfile))
{
    croak "Could not open $tmpfile for read/write: $!";
}
$ofh->autoflush(1);

$faux_res = RPC::XML::response->new(RPC::XML::fault->new(1, 'test'));

is(length($faux_res->as_string), $faux_res->length,
   'length() in fault response');

$faux_res->serialize($ofh);
# Again, this means that all the triggered calls managed to not die
ok(1, 'serialize method did not croak');

is(-s $ofh, length($faux_res->as_string), 'Fault-response file size OK');

seek $ofh, 0, 0;
$data = q{};
read $ofh, $data, -s $ofh;

# There have been some changes to how Perl handles iteration of hash keys.
# As a result, this test has started failing a lot because of the order of
# keys when serialized doesn't match the order of keys from as_string(). So
# to get around this, just compare it to both variations that can occur.
my $variant1 = '<?xml version="1.0" encoding="us-ascii"?><methodResponse>' .
    '<fault><value><struct><member><name>faultString</name><value><string>' .
    'test</string></value></member><member><name>faultCode</name><value>' .
    '<int>1</int></value></member></struct></value></fault></methodResponse>';
my $variant2 = '<?xml version="1.0" encoding="us-ascii"?><methodResponse>' .
    '<fault><value><struct><member><name>faultCode</name><value><int>1</int>' .
    '</value></member><member><name>faultString</name><value><string>test' .
    '</string></value></member></struct></value></fault></methodResponse>';
ok(
    ($data eq $variant1) || ($data eq $variant2),
    'Fault-response content is correct'
);

close $ofh;
unlink $tmpfile;

# Round two, with normal response (not fault)
if (! (open $ofh, '+>', $tmpfile))
{
    croak "Could not open $tmpfile for read/write: $!";
}
$ofh->autoflush(1);

$faux_res = RPC::XML::response->new('test');

is(length($faux_res->as_string), $faux_res->length,
   'length() in normal response');

$faux_res->serialize($ofh);
# Again, this means that all the triggered calls managed to not die
ok(1, 'serialize method did not croak');

is(-s $ofh, length($faux_res->as_string), 'Normal response file size OK');

seek $ofh, 0, 0;
$data = q{};
read $ofh, $data, -s $ofh;

is($data, $faux_res->as_string, 'Normal response content OK');

close $ofh;
unlink $tmpfile;

# Test some extra code-paths in the base64 logic:

# Route 1: In-memory content
if (! (open $ofh, '+>', $tmpfile))
{
    croak "Could not open $tmpfile for read/write: $!";
}
$ofh->autoflush(1);

$faux_res = RPC::XML::response->new(RPC::XML::base64->new('a simple string'));

is(length($faux_res->as_string), $faux_res->length,
   'length() in normal response');

$faux_res->serialize($ofh);
# Again, this means that all the triggered calls managed to not die
ok(1, 'serialize method did not croak');

is(-s $ofh, length($faux_res->as_string), 'Normal response file size OK');

seek $ofh, 0, 0;
$data = q{};
read $ofh, $data, -s $ofh;

is($data, $faux_res->as_string, 'Normal response content OK');

close $ofh;
unlink $tmpfile;

# Route 2: Spool from a file that is already encoded
if (! (open $ofh, '+>', $tmpfile))
{
    croak "Could not open $tmpfile for read/write: $!";
}
$ofh->autoflush(1);

$file = File::Spec->catfile($dir, 'svsm_text.b64');
if (! (open $fh, '<', $file))
{
    croak "Could not open $file for reading: $!";
}
$faux_res = RPC::XML::response->new(RPC::XML::base64->new($fh, 'encoded'));

is(length($faux_res->as_string), $faux_res->length,
   'length() in normal response');

$faux_res->serialize($ofh);
# Again, this means that all the triggered calls managed to not die
ok(1, 'serialize method did not croak');

# If we're on Windows, then the re-spooling of the content of svsm_text.b64
# introduced 32 extra bytes (due to \n\r silliness). Set $offset to 0 or 32
# depending on the value of $^O.
my $offset = ($^O =~ /mswin/i) ? 32 : 0;
is(-s $ofh, length($faux_res->as_string) + $offset,
   'Normal response file size OK');

seek $ofh, 0, 0;
$data = q{};
read $ofh, $data, -s $ofh;

is($data, $faux_res->as_string, 'Normal response content OK');

close $fh;
close $ofh;
unlink $tmpfile;

exit;
