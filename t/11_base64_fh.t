#!/usr/bin/perl

# Test the usage of RPC::XML::base64 with filehandles

use strict;
use vars qw($dir $file $tmpfile $value $enc_value $obj $fh $pos $md5_able);

# This is what we're testing
use RPC::XML;

use Test;
use File::Spec;
use IO::File;
use MIME::Base64;

(undef, $dir, undef) = File::Spec->splitpath($0);
$file = File::Spec->catfile($dir, 'svsm_text.gif');
$tmpfile = File::Spec->catfile($dir, "__tmp__${$}__");

BEGIN
{
    eval "use Digest::MD5";
    $md5_able = $@ ? 0 : 1;

    plan tests => 22;
}

END
{
    unlink $tmpfile;
}

$value = 'Short string for easy tests';
$enc_value = encode_base64($value);

if (ref($fh = IO::File->new_tmpfile))
{
    $fh->autoflush(1);
    print $fh $value;
    $pos = $fh->tell;

    # We now have a ready-to-use FH, and we know the seek-pos on it
    $obj = RPC::XML::base64->new($fh);
    ok(ref $obj);
    ok($fh->tell() == $pos);
    ok($obj->value() eq $value);
    ok($fh->tell() == $pos);
    ok($obj->as_string() eq "<base64>$enc_value</base64>");
    ok($fh->tell() == $pos);

    # Done with this for now
    $fh->close;
}
else
{
    skip('Skipped: opening IO::File::new_tmpfile failed') for (1 .. 6);
}

# Same tests, but init the FH with the encoded data rather than the cleartext
if (ref($fh = IO::File->new_tmpfile))
{
    $fh->autoflush(1);
    print $fh $enc_value;
    $pos = $fh->tell;

    # We now have a ready-to-use FH, and we know the seek-pos on it
    $obj = RPC::XML::base64->new($fh, 'encoded');
    ok(ref $obj);
    ok($fh->tell() == $pos);
    ok($obj->value() eq $value);
    ok($fh->tell() == $pos);
    ok($obj->as_string() eq "<base64>$enc_value</base64>");
    ok($fh->tell() == $pos);

    # Done with this for now
    $fh->close;
}
else
{
    skip('Skipped: opening IO::File::new_tmpfile failed') for (1 .. 6);
}

# Test ordinary filehandles as well
local *F;
if (open(F, "> $tmpfile"))
{
    print F $value;
    close(F);
}
else
{
    die "Cannot create local file $tmpfile: $!";
}

if (open(F, "< $tmpfile"))
{
    autoflush F 1;
    print F $value;
    seek(F, 0, 0);
    $pos = tell(F);

    $obj = RPC::XML::base64->new(\*F);
    ok(ref $obj);
    ok(tell(F) == $pos);
    ok($obj->value() eq $value);
    ok(tell(F) == $pos);
    ok($obj->as_string() eq "<base64>$enc_value</base64>");
    ok(tell(F) == $pos);

    close(F);
}
else
{
    skip("Skipped: Opening $tmpfile failed: $!") for (1 .. 6);
}

# Test with a larger file
if ($fh = IO::File->new("< $file"))
{
    $obj = RPC::XML::base64->new($fh);
    $enc_value = ''; $value = '';

    while (read($fh, $value, 60*57))
    {
        $enc_value .= encode_base64($value);
    }
    ok($obj->as_string(), "<base64>$enc_value</base64>");
    $fh->seek(0, 0);

    if ($md5_able)
    {
        my $md5 = Digest::MD5->new;

        $md5->addfile($fh);
        $value = $md5->hexdigest;
        $md5->new; # Clear the digest
        $md5->add($obj->value);
        ok($value, $md5->hexdigest);
    }
    else
    {
        skip("Skipped: Digest::MD5 unavailable");
    }

    $fh->close;
}
else
{
    skip("Skipped: Opening file $file failed: $!") for (1 .. 2);
}

# Test the to_file method
if ($md5_able and $fh = IO::File->new("< $file"))
{
    $obj = RPC::XML::base64->new($fh);

    my $md5 = Digest::MD5->new;

    # Start by trying to write the new file
    $obj->to_file($tmpfile);
    ok(1); # Whee! It didn't die!

    $md5->addfile($fh);
    $value = $md5->hexdigest;
    $md5->new; # Clear the digest

    # Now get an MD5 on the new file
    my $ofh;
    die "Error opening $tmpfile for reading: $!"
        unless ($ofh = IO::File->new("< $tmpfile"));
    $md5->addfile($ofh);
    ok($value, $md5->hexdigest);
    $ofh->close;
    $fh->close;
}
else
{
    skip("Skipped: Digest::MD5 not availabe or open of $file failed")
        for (1 .. 2);
}

exit;
