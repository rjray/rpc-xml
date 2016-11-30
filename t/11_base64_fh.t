#!/usr/bin/env perl

# Test the usage of RPC::XML::base64 with filehandles

## no critic(RequireBriefOpen)
## no critic(RequireCheckedClose)
## no critic(RequireInterpolationOfMetachars)

use strict;
use warnings;

use Carp qw(croak carp);
use Test::More;
use File::Spec;
use IO::Handle; # Allow "$fh->autoflush(1)" for setting $|
use Digest::MD5;
use MIME::Base64;

# This is what we're testing
use RPC::XML;

my ($dir, $vol, $file, $b64file, $tmpfile, $value, $enc_value, $obj, $fh, $pos,
    $md5_able, $md5, $size, $ofh);

plan tests => 35;

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, q{});
$file = File::Spec->catfile($dir, 'svsm_text.gif');
$b64file = File::Spec->catfile($dir, 'svsm_text.b64');
$tmpfile = File::Spec->catfile($dir, "__tmp__${$}__");

END
{
    if (-f $tmpfile)
    {
        unlink $tmpfile;
    }
}

$value = 'Short string for easy tests';
$enc_value = encode_base64($value, q{});

if (! (open $fh, '+>', $tmpfile))
{
    croak "Error opening $tmpfile: $!";
}

$fh->autoflush(1);

print {$fh} $value;
$pos = tell $fh;

# We now have a ready-to-use FH, and we know the seek-pos on it
$obj = RPC::XML::base64->new($fh);
isa_ok($obj, 'RPC::XML::base64', '$obj');
is(tell $fh, $pos, 'object construction leaves pos() unchanged');
is($obj->value(), $value, 'object value is correct');
is(tell $fh, $pos, 'call to value() leaves pos() unchanged');
is($obj->as_string(), "<base64>$enc_value</base64>",
   'object stringification is correct');
is(tell $fh, $pos, 'call to as_string leaves pos() unchanged');

# Done with this for now
close $fh;
unlink $tmpfile;

# Same tests, but init the FH with the encoded data rather than the cleartext
if (! (open $fh, '+>', $tmpfile))
{
    croak "Error opening $tmpfile: $!";
}

$fh->autoflush(1);

print {$fh} $enc_value;
$pos = tell $fh;

# We now have a ready-to-use FH, and we know the seek-pos on it
$obj = RPC::XML::base64->new($fh, 'encoded');
isa_ok($obj, 'RPC::XML::base64', '$obj(encoded)');
is(tell $fh, $pos, 'object(encoded) construction leaves pos() unchanged');
is($obj->value(), $value, 'object(encoded) value is correct');
is(tell $fh, $pos, 'call to value() leaves pos() unchanged');
is($obj->as_string(), "<base64>$enc_value</base64>",
   'object(encoded) stringification is correct');
is(tell $fh, $pos, 'call to as_string leaves pos() unchanged');

# Done with this for now
close $fh;
unlink $tmpfile;

# Test old-style glob filehandles
{
    ## no critic(ProhibitBarewordFilehandles)
    ## no critic(RequireBracedFileHandleWithPrint)

    if (! (open F, '+>', $tmpfile))
    {
        croak "Error opening $tmpfile: $!";
    }

    F->autoflush(1);

    print F $enc_value;
    $pos = tell F;

    # We now have a ready-to-use FH, and we know the seek-pos on it
    $obj = RPC::XML::base64->new(\*F, 'encoded');
    isa_ok($obj, 'RPC::XML::base64', '$obj(glob)');
    is(tell F, $pos, 'object(glob) construction leaves pos() unchanged');
    is($obj->value(), $value, 'object(glob) value is correct');
    is(tell F, $pos, 'call to value() leaves pos() unchanged');
    is($obj->as_string(), "<base64>$enc_value</base64>",
       'object(glob) stringification is correct');
    is(tell F, $pos, 'call to as_string leaves pos() unchanged');

    # Done with this for now
    close F;
    unlink $tmpfile;
}

# Test with a larger file
if (! (open $fh, '<', $file))
{
    croak "Error opening $file: $!";
}

$obj = RPC::XML::base64->new($fh);
isa_ok($obj, 'RPC::XML::base64', '$obj');
$enc_value = q{}; $value = q{};

while (read $fh, $value, 60*57)
{
    $enc_value .= encode_base64($value, q{});
}
is($obj->as_string(), "<base64>$enc_value</base64>",
   'from file, stringification');
is(length($obj->as_string), $obj->length, 'from file, length');
seek $fh, 0, 0;

$md5 = Digest::MD5->new;
$md5->addfile($fh);
$value = $md5->hexdigest;
$md5->new; # Clear the digest
$md5->add($obj->value);
is($value, $md5->hexdigest, 'MD5 checksum matches');

close $fh;

# Test the to_file method
if (! (open $fh, '<', $file))
{
    croak "Error opening $file: $!";
}
$obj = RPC::XML::base64->new($fh);

# Start by trying to write the new file
$size = $obj->to_file($tmpfile);
is($size, -s $file, 'to_file call returned correct number of bytes');
is(-s $tmpfile, -s $file, 'temp-file size matches file size');

$md5 = Digest::MD5->new;
$md5->addfile($fh);
$value = $md5->hexdigest;
$md5->new; # Clear the digest

# Now get an MD5 on the new file
if (! (open $ofh, '<', $tmpfile))
{
    croak "Error opening $tmpfile for reading: $!";
}
$md5->addfile($ofh);
is($value, $md5->hexdigest, 'MD5 hexdigest matches');
close $ofh;
unlink $tmpfile;
close $fh;

# Try with in-memory data
$value = 'a simple in-memory string';
$obj = RPC::XML::base64->new($value);
# Try to write it
$size = $obj->to_file($tmpfile);
is($size, length $value, 'to_file call returned correct number of bytes');
is(length $value, -s $tmpfile, 'temp-file size matches string');
unlink $tmpfile;

# Try with a file-handle instead of a file name
if (! (open $ofh, '>', $tmpfile))
{
    croak "Error opening $tmpfile for writing: $!";
}
$ofh->autoflush(1);
$size = $obj->to_file($ofh);
is($size, length $value, 'to_file call on file-handle, correct size');
is(length $value, -s $ofh, 'temp-file size matches string');
close $ofh;
unlink $tmpfile;

# Try an unusable reference
$size = $obj->to_file([]);
is($size, -1, 'to_file call failed on unusable reference type');
like($RPC::XML::ERROR, qr/Unusable reference/, 'Correct error message');

SKIP: {
    # Test the failure to open a file. Cannot run this on Windows because
    # it doesn't have the concept of chmod...
    if ($^O eq 'MSWin32' || $^O eq 'cygwin')
    {
        skip 'Tests involving directory permissions skipped on Windows', 2;
    }
    # ...nor can we run it as root, because root.
    if ($< == 0)
    {
        skip 'Tests involving directory permissions skipped under root', 2;
    }

    my $baddir = File::Spec->catdir(File::Spec->tmpdir(), "baddir_$$");
    if (! mkdir $baddir)
    {
        skip "Skipping, failed to create dir $baddir: $!", 2;
    }
    if (! chmod oct(600), $baddir)
    {
        skip "Skipping, failed to chmod dir $baddir: $!", 2;
    }
    my $badfile = File::Spec->catfile($baddir, 'file');

    $size = $obj->to_file($badfile);
    is($size, -1, 'to_file call failed on un-openable file');
    like($RPC::XML::ERROR, qr/Error opening/, 'Correct error message');

    if (! rmdir $baddir)
    {
        carp "Failed to remove temp-dir $baddir: $!";
    }
}

# Test to_file() with an encoded file in the file-handle
if (! (open $fh, '<', $b64file))
{
    croak "Error opening $b64file for reading: $!";
}
$obj = RPC::XML::base64->new($fh, 'encoded');
$size = $obj->to_file($tmpfile);
is($size, -s $file, 'to_file() written size matches decoded file size');
if (! (open $fh, '<', $file))
{
    croak "Error opening $file: $!";
}
$md5 = Digest::MD5->new;
$md5->addfile($fh);
$value = $md5->hexdigest;
$md5->new; # Clear the digest

# Now get an MD5 on the new file
if (! (open $ofh, '<', $tmpfile))
{
    croak "Error opening $tmpfile for reading: $!";
}
$md5->addfile($ofh);
is($value, $md5->hexdigest, 'MD5 hexdigest matches');
close $ofh;
unlink $tmpfile;
close $fh;

exit;
