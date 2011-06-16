use strict;
use warnings;

use HTTP::Request;
use Test::More;

sub clone_with_host_header {
    my $req      = shift;
    my $reqclone = $req->clone;
    unless ( $reqclone->header('Host') ) {
        $reqclone->header( Host => URI->new( $reqclone->uri )->host );
    }
    return $reqclone;
}

subtest "without_host_header" => sub {
    my $req = HTTP::Request->new( POST => 'http://example.com' );
    ok( !$req->header('Host') );

    my $reqclone = clone_with_host_header($req);
    ok( $reqclone->header('Host'), 'example.com' );
};

subtest "with_host_hedar" => sub {
    my $req = HTTP::Request->new( POST => 'http://example.com' );
    ok( !$req->header('Host') );
    $req->header( 'Host', 'google.com' );
    is( $req->header('Host'), 'google.com' );

    my $reqclone = clone_with_host_header($req);
    ok( $reqclone->header('Host'), 'google.com' );
};

done_testing;
