#!/usr/bin/env perl

# Test the ability of requests to specify their own Host: header

use strict;
use warnings;

use HTTP::Request;
use Test::More;

plan tests => 2;

sub clone_with_host_header
{
    my $req      = shift;
    my $reqclone = $req->clone;

    if (! $reqclone->header('Host'))
    {
        $reqclone->header(Host => URI->new($reqclone->uri)->host);
    }

    return $reqclone;
}

subtest 'without_host_header' => sub {
    plan tests => 2;

    my $req = HTTP::Request->new(POST => 'http://example.com');
    ok(! $req->header('Host'), 'Host: header not set');

    my $reqclone = clone_with_host_header($req);
    is($reqclone->header('Host'), 'example.com', 'Host: header set properly');
};

subtest 'with_host_header' => sub {
    plan tests => 3;

    my $req = HTTP::Request->new(POST => 'http://example.com');
    ok(! $req->header('Host'), 'Host: header not set');
    $req->header('Host', 'google.com');
    is($req->header('Host'), 'google.com', 'Host: header set properly');

    my $reqclone = clone_with_host_header($req);
    is($reqclone->header('Host'), 'google.com',
       'Host: header in clone is correct');
};

exit;
