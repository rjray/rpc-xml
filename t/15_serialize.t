#!/usr/bin/perl

# Test the serialization of XML structures to filehandles

use strict;
use vars qw($dir $vol $fh $file $tmpfile $md5_able $faux_req $faux_res $ofh
            $data);

use RPC::XML ':all';

use Test::More tests => 20;
use File::Spec;

# We'll be using the <nil /> extension here:
$RPC::XML::ALLOW_NIL = 1;

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, '');
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
    die "Could not open $file for reading: $!";
}

$faux_req = RPC::XML::request->new(
    'test',
    RPC_STRING 'string',
    RPC_INT 10,
    RPC_I4 20,
    RPC_I8 4294967296,
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
    die "Could not open $tmpfile for read/write: $!";
}
select $ofh; $| = 1; select STDOUT;

$faux_req->serialize($ofh);
ok(1, 'serialize method did not croak'); # Just happy we made it this far.

is(-s $ofh, length($faux_req->as_string), 'File size is correct');

seek $ofh, 0, 0;
$data = '';
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
    die "Could not open $tmpfile for read/write: $!";
}
select $ofh; $| = 1; select STDOUT;

$faux_res = RPC::XML::response->new(RPC::XML::fault->new(1, 'test'));

is(length($faux_res->as_string), $faux_res->length,
   'length() in fault response');

$faux_res->serialize($ofh);
# Again, this means that all the triggered calls managed to not die
ok(1, 'serialize method did not croak');

is(-s $ofh, length($faux_res->as_string), 'Fault-response file size OK');

seek $ofh, 0, 0;
$data = '';
read $ofh, $data, -s $ofh;

is($data, $faux_res->as_string, 'Fault-response content is correct');

close $ofh;
unlink $tmpfile;

# Round two, with normal response (not fault)
if (! (open $ofh, '+>', $tmpfile))
{
    die "Could not open $tmpfile for read/write: $!";
}
select $ofh; $| = 1; select STDOUT;

$faux_res = RPC::XML::response->new('test');

is(length($faux_res->as_string), $faux_res->length,
   'length() in normal response');

$faux_res->serialize($ofh);
# Again, this means that all the triggered calls managed to not die
ok(1, 'serialize method did not croak');

is(-s $ofh, length($faux_res->as_string), 'Normal response file size OK');

seek $ofh, 0, 0;
$data = '';
read $ofh, $data, -s $ofh;

is($data, $faux_res->as_string, 'Normal response content OK');

close $ofh;
unlink $tmpfile;

# Test some extra code-paths in the base64 logic:

# Route 1: In-memory content
if (! (open $ofh, '+>', $tmpfile))
{
    die "Could not open $tmpfile for read/write: $!";
}
select $ofh; $| = 1; select STDOUT;

$faux_res = RPC::XML::response->new(RPC::XML::base64->new('a simple string'));

is(length($faux_res->as_string), $faux_res->length,
   'length() in normal response');

$faux_res->serialize($ofh);
# Again, this means that all the triggered calls managed to not die
ok(1, 'serialize method did not croak');

is(-s $ofh, length($faux_res->as_string), 'Normal response file size OK');

seek $ofh, 0, 0;
$data = '';
read $ofh, $data, -s $ofh;

is($data, $faux_res->as_string, 'Normal response content OK');

close $ofh;
unlink $tmpfile;

# Route 2: Spool from a file that is already encoded
if (! (open $ofh, '+>', $tmpfile))
{
    die "Could not open $tmpfile for read/write: $!";
}
select $ofh; $| = 1; select STDOUT;

$file = File::Spec->catfile($dir, 'svsm_text.b64');
if (! (open $fh, '<', $file))
{
    die "Could not open $file for reading: $!";
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
$data = '';
read $ofh, $data, -s $ofh;

is($data, $faux_res->as_string, 'Normal response content OK');

close $fh;
close $ofh;
unlink $tmpfile;

exit;
