#!/usr/bin/perl

# Test the serialization of XML structures to filehandles

use strict;
use vars qw($dir $fh $file $tmpfile $md5_able $faux_req $faux_res $ofh $data);

use RPC::XML ':all';

use Test;
use File::Spec;
use IO::File;

(undef, $dir, undef) = File::Spec->splitpath($0);
$file = File::Spec->catfile($dir, 'svsm_text.gif');
$tmpfile = File::Spec->catfile($dir, "__tmp__${$}__");

BEGIN
{
    eval "use Digest::MD5";
    $md5_able = $@ ? 0 : 1;

    plan tests => 8;
}

END
{
    unlink $tmpfile;
}

die "Could not open $file for reading: $!"
    unless $fh = IO::File->new("< $file");

$faux_req = RPC::XML::request->new('test',
				   RPC_STRING  'string',
				   RPC_INT     10,
				   RPC_I4      20,
				   RPC_BOOLEAN 1,
				   [ qw(a b c) ],
				   { one => 2 },
				   RPC_BASE64 $fh);

# This is a good place to test the length() method, while we're at it
ok(length($faux_req->as_string), $faux_req->length);

die "Could not open $tmpfile for read/write: $!"
    unless $ofh = IO::File->new("+> $tmpfile");
$ofh->autoflush(1);

$faux_req->serialize($ofh);
ok(1); # Just happy we made it this far.

ok(-s $ofh, length($faux_req->as_string));

$ofh->seek(0, 0);
$data = '';
read($ofh, $data, -s $ofh);

ok($data, $faux_req->as_string);

# Done with these for now
$ofh->close; unlink $tmpfile;
$fh->close;

die "Could not open $tmpfile for read/write: $!"
    unless $ofh = IO::File->new("+> $tmpfile");
$ofh->autoflush(1);

$faux_res = RPC::XML::response->new(RPC::XML::fault->new(1, 'test'));

# This is a good place to test the length() method, while we're at it
ok(length($faux_res->as_string), $faux_res->length);

$faux_res->serialize($ofh);
ok(1); # Again, this means that all the triggered calls managed to not die

ok(-s $ofh, length($faux_res->as_string));

$ofh->seek(0, 0);
$data = '';
read($ofh, $data, -s $ofh);

ok($data, $faux_res->as_string);

$ofh->close;

exit;
