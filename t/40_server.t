#!/usr/bin/perl

# Test the RPC::XML::Server class

use strict;
use subs qw(start_server find_port);
use vars qw($srv $res $bucket $child $parser $xml $req $port $UA @API_METHODS);

use Test;

use IO::Socket;
use LWP::UserAgent;
use HTTP::Request;

use RPC::XML::Server;
use RPC::XML::Parser;

BEGIN { plan tests => 25 }

@API_METHODS = qw(system.identity system.introspection system.listMethods
                  system.methodHelp system.methodSignature system.multicall
                  system.status);

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# any other classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Server. In this particular case, this
# also means that we cannot use RPC::XML::Client to test it.

# Start with some very basic things, without actually firing up a live server.
$srv = RPC::XML::Server->new(no_http => 1, no_default => 1);

ok(ref($srv) eq 'RPC::XML::Server');
ok($srv->version eq $RPC::XML::Server::VERSION);
ok(! $srv->started);
ok($srv->product_tokens =~ m|/|);
ok(! $srv->url);
ok(! $srv->requests);
ok($srv->response->isa('HTTP::Response'));
# We're done with this one, let it go
undef $srv;

# This one will have a HTTP::Daemon server, but still no default methods
die "No usable port found between 9000 and 10000, skipping"
    if (($port = find_port) == -1);
$srv = RPC::XML::Server->new(no_default => 1, port => $port);
ok(ref($srv) eq 'RPC::XML::Server');
ok($srv->url); # This should be non-null this time
# Test some of the simpler cases of add_method and get_method
$res = $srv->add_method({ name      => 'perl.test.suite.test1',
                          signature => [ 'int' ],
                          code      => sub { return 1; } });
ok($res eq $srv);
$res = $srv->get_method('perl.test.suite.test1');
ok($res and (ref($res) eq 'RPC::XML::Method'));
$res = $srv->get_method('perl.test.suite.not.added.yet');
ok(! ref($res));
# Here goes...
$parser = RPC::XML::Parser->new;
$UA = LWP::UserAgent->new;
$req = HTTP::Request->new(POST => "http://localhost:$port/");
$xml = qq(<?xml version="1.0"?>
<methodCall>
<methodName>perl.test.suite.test1</methodName>
<params></params>
</methodCall>);
$child = start_server($srv);

$req->header(Content_Type => 'text/xml');
$req->content($xml);
# Use alarm() to manage a resaonable time-out on the request
$bucket = 0;
$SIG{ALRM} = sub { $bucket++ };
alarm(120);
$res = $UA->request($req);
alarm(0);
if ($bucket)
{
    print STDERR "Server failed to respond within 120 seconds!\n";
    ok(0); # Match the number of tests in the alternate block
    ok(0);
    ok(0);
    ok(0);
}
else
{
    ok(! $res->is_error);
    $xml = $res->content;
    $res = $parser->parse($xml);
    ok(ref($res) eq 'RPC::XML::response');
    ok(! $res->is_fault);
    ok($res->value->value == 1);
}
kill 'INT', $child;

# Try deleting the method
ok(ref $srv->delete_method('perl.test.suite.test1'));

# Start the server again
$child = start_server($srv);
$bucket = 0;
$SIG{ALRM} = sub { $bucket++ };
alarm(120);
$res = $UA->request($req);
alarm(0);
if ($bucket)
{
    print STDERR "Server failed to respond within 120 seconds!\n";
    ok(0); # Match the number of tests in the alternate block
    ok(0);
    ok(0);
    ok(0);
}
else
{
    ok(! $res->is_error);
    $res = $parser->parse($res->content);
    ok(ref($res) eq 'RPC::XML::response');
    ok($res->is_fault);
    ok($res->value->value->{faultString} =~ /Unknown method/);
}
kill 'INT', $child;

# OK-- At this point, basic server creation and accessors have been validated.
# We've run a remote method and we've correctly failed to run an unknown remote
# method. Before moving into the more esoteric XPL-file testing, we will test
# the provided introspection API.
undef $srv;
undef $req;
die "No usable port found between 9000 and 10000, skipping"
    if (($port = find_port) == -1);
$srv = RPC::XML::Server->new(port => $port);

# Did it create OK, with the requirement of loading the XPL code?
if (ref $srv)
{
    ok(1);
    # Did it get all of them?
    ok($srv->list_methods() == @API_METHODS);
    $req = HTTP::Request->new(POST => "http://localhost:$port/");
    $xml = qq(<?xml version="1.0"?>
<methodCall>
<methodName>system.listMethods</methodName>
<params></params>
</methodCall>);

    $child = start_server($srv);

    $req->header(Content_Type => 'text/xml');
    $req->content($xml);
    # Use alarm() to manage a resaonable time-out on the request
    $bucket = 0;
    undef $res;
    $SIG{ALRM} = sub { $bucket++ };
    alarm(120);
    $res = $UA->request($req);
    alarm(0);
    if ($bucket)
    {
        print STDERR "Server failed to respond within 120 seconds!\n";
        ok(0); # Match the number of tests in the alternate block
        ok(0);
    }
    else
    {
	$res = ($res->is_error) ? '' : $parser->parse($res->content);
        ok(ref($res) eq 'RPC::XML::response');
        my $list = (ref $res) ? $res->value->value : [];
        ok((ref($list) eq 'ARRAY') &&
           (join('', sort @$list) eq join('', sort @API_METHODS)));
    }
}
else
{
    ok(0);
    ok(0);
    ok(0);
    ok(0);
}
kill 'INT', $child;

exit;

sub start_server
{
    my $S = shift;

    my $pid;

    if (! defined($pid = fork()))
    {
        die "fork() error: $!, stopped";
    }
    elsif ($pid)
    {
        return $pid;
    }
    else
    {
        $S->server_loop();
        exit; # When the parent stops this server, we want to stop this child
    }
}

sub find_port
{
    my $start_at = $_[0] || 9000;

    my ($port, $sock);

    for ($port = $start_at; $port < ($start_at + 1000); $port++)
    {
        $sock = IO::Socket->new(Domain   => AF_INET,
                                PeerAddr => 'localhost',
                                PeerPort => $port);
        return $port unless ref $sock;
    }

    -1;
}
