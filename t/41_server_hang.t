#!/usr/bin/env perl

# Test the RPC::XML::Server bug that causes a hang when a client terminates in
# mid-message. Unlike 40_server.t, this isn't trying to fully exercise the
# server class, just looking for and (trying to) tickle a specific bug.

## no critic(RequireCheckedClose)

use strict;
use warnings;
use subs qw(start_server);

use Carp qw(carp croak);
use File::Spec;
use IO::Socket::IP;
use Test::More;

use HTTP::Request;

use RPC::XML::Server;

my ($dir, $vol, $srv, $bucket, $child, $req, $port, $socket, $body);

plan tests => 2;

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, q{});
require File::Spec->catfile($dir, 'util.pl');

{
    package MyServer;

    use strict;
    use base qw(RPC::XML::Server);

    sub process_request
    {
        my $self = shift;
        $self->SUPER::process_request(@_);

        exit 1;
    }
}

SKIP: {
    if ($^O eq 'MSWin32' || $^O eq 'cygwin')
    {
        skip 'This suite does not run on MSWin/cygwin', 2;
    }

    $srv = MyServer->new(no_default => 1);
    isa_ok($srv, 'RPC::XML::Server', 'Server instance');
    $srv->add_method({ name      => 'echo',
                       signature => [ 'string string' ],
                       code      => sub { shift; return shift; } });

    $port = $srv->port;
    $req = HTTP::Request->new(POST => "http://localhost:$port/");
    $body = RPC::XML::request->new('echo', 'foo')->as_string;
    $req->content($body);
    $req->protocol('HTTP/1.0');
    $req->header(Content_Length => length $body);
    $req->header(Content_Type => 'text/xml');
    $req = $req->as_string;
    $req = substr $req, 0, (length($req) - 32);

    $child = start_server $srv;
    $bucket = 0;
    local $SIG{CHLD} = sub {
        my $dead = wait;
        if ($dead == $child)
        {
            $bucket = $? >> 8;
        }
        else
        {
            carp 'PANIC: Unknown child return';
        }
    };

    # Create an IO::Socket object for the client-side. In order to fool the
    # server with a bad Content-Length and terminate early, we have to ditch
    # LWP and go old-skool.
    $socket = IO::Socket::IP->new(Proto => 'tcp', PeerAddr => 'localhost',
                                    PeerPort => $port)
        or croak "Error creating IO::Socket obj: $!";
    print {$socket} $req;
    # This *should* force the server to drop the request. The bug relates to
    # the fact that (previously) the server just hangs:
    close $socket;

    # Give the server time to crap out:
    if (! $bucket)
    {
        sleep 95;
    }

    # If it still hasn't, kill it:
    local $SIG{CHLD} = 'IGNORE';
    if (! $bucket)
    {
        kill 'KILL', $child;
    }

    is($bucket, 1, 'Check if server hangs on short requests');
}

exit;
