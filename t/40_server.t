#!/usr/bin/perl

# Test the RPC::XML::Server class

use strict;
use subs qw(start_server find_port);
use vars qw($srv $res $bucket $child $parser $xml $req $port $UA @API_METHODS
            $list $meth @keys %seen $dir $vol);

use Socket;
use File::Spec;

use Test::More tests => 66;
use LWP::UserAgent;
use HTTP::Request;
use Scalar::Util 'blessed';

use RPC::XML 'RPC_BASE64';
require RPC::XML::Server;
require RPC::XML::ParserFactory;

@API_METHODS = qw(system.identity system.introspection system.listMethods
                  system.methodHelp system.methodSignature system.multicall
                  system.status);

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, '');
require File::Spec->catfile($dir, 'util.pl');

sub failmsg { sprintf("%s at line %d", @_) }

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# any other classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Server. In this particular case, this
# also means that we cannot use RPC::XML::Client to test it.

# Start with some very basic things, without actually firing up a live server.
$srv = RPC::XML::Server->new(no_http => 1, no_default => 1);

isa_ok($srv, 'RPC::XML::Server', '$srv<1>');
# Suppress "used only once" warning
$_ = $RPC::XML::Server::VERSION;
is($srv->version, $RPC::XML::Server::VERSION,
   'RPC::XML::Server::version method');
ok(! $srv->started, 'RPC::XML::Server::started method');
like($srv->product_tokens, qr|/|, 'RPC::XML::Server::product_tokens method');
ok(! $srv->url, 'RPC::XML::Server::url method (empty)');
ok(! $srv->requests, 'RPC::XML::Server::requests method (0)');
ok($srv->response->isa('HTTP::Response'),
   'RPC::XML::Server::response method returns HTTP::Response');
# Done with this one, let it go
undef $srv;

# This one will have a HTTP::Daemon server, but still no default methods
die "No usable port found between 9000 and 10000, skipping"
    if (($port = find_port) == -1);
$srv = RPC::XML::Server->new(no_default => 1,
                             host => 'localhost', port => $port);
isa_ok($srv, 'RPC::XML::Server', '$srv<2>');
# Test the URL the server uses. Allow for "localhost", "localhost.localdomain"
# or the local-net IP address of this host (not always 127.0.0.1).
# 22/09/2008 - Just allow for anything the user has attached to this address.
#              Aliases keep causing this test to falsely fail.
my @localhostinfo = gethostbyname('localhost');
my $localIP = join('.', unpack('C4', $localhostinfo[4]));
my @allhosts = ($localIP, $localhostinfo[0], split(' ', $localhostinfo[1]));
for (@allhosts) { s/\./\\./g }
# Per RT 27778: For some reason gethostbyname('localhost') does not return
# "localhost" on win32
push @allhosts, 'localhost' if ($^O eq 'MSWin32' || $^O eq 'cygwin');
push @allhosts, 'localhost\.localdomain'
    unless (grep(/localdomain/, @allhosts));
my $allhosts = join('|', @allhosts);
like($srv->url, qr{http://($allhosts):$port},
   'RPC::XML::Server::url method (set)'); # This should be non-null this time
# Test some of the simpler cases of add_method and get_method
$res = $srv->add_method({ name      => 'perl.test.suite.test1',
                          signature => [ 'int' ],
                          code      => sub { return 1; } });
ok($res eq $srv, 'add_method return value test');
$res = $srv->get_method('perl.test.suite.test1');
isa_ok($res, 'RPC::XML::Method', 'get_method return value');
$res = $srv->get_method('perl.test.suite.not.added.yet');
ok(! ref($res), 'get_method for non-existent method');
# Here goes...
$parser = RPC::XML::ParserFactory->new;
$UA = LWP::UserAgent->new;
$req = HTTP::Request->new(POST => "http://localhost:$port/");
$child = start_server($srv);

$req->header(Content_Type => 'text/xml');
$req->content(RPC::XML::request->new('perl.test.suite.test1')->as_string);
# Use alarm() to manage a resaonable time-out on the request
$bucket = 0;
$SIG{ALRM} = sub { $bucket++ };
alarm(120);
$res = $UA->request($req);
alarm(0);
ok(! $bucket, 'First live-request returned without timeout');
SKIP: {
    skip "Server failed to respond within 120 seconds!", 4 if $bucket;

    ok(! $res->is_error, 'First live req: Check that $res is not an error');
    $xml = $res->content;
    $res = $parser->parse($xml);
    isa_ok($res, 'RPC::XML::response', 'First live req: parsed $res');
  SKIP: {
        skip "Response content did not parse, cannot test", 2
            unless (ref $res and $res->isa('RPC::XML::response'));
        ok(! $res->is_fault, 'First live req: parsed $res is not a fault');
        is($res->value->value, 1, 'First live req: $res value test');
    }
}
stop_server($child);

# Try deleting the method
ok(ref $srv->delete_method('perl.test.suite.test1'),
   'delete_method return value test');

# Start the server again
# Add a method that echoes back socket-peer information
$res = $srv->add_method({ name      => 'perl.test.suite.peeraddr',
                          signature => [ 'array' ],
                          code      =>
                          sub {
                              my $srv = shift;

                              my $ipaddr = inet_aton($srv->{peerhost});
                              my $peeraddr = RPC_BASE64 $srv->{peeraddr};
                              my $packet = pack_sockaddr_in($srv->{peerport},
                                                            $ipaddr);
                              $packet = RPC_BASE64 $packet;
                              [ $peeraddr, $packet,
                                $srv->{peerhost}, $srv->{peerport} ];
                          } });
$child = start_server($srv);
$bucket = 0;
$SIG{ALRM} = sub { $bucket++ };
alarm(120);
$res = $UA->request($req);
alarm(0);
ok(! $bucket, 'Second live-request returned without timeout');
SKIP: {
    skip "Server failed to respond within 120 seconds!", 4 if $bucket;

    ok(! $res->is_error, 'Second live req: Check that $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'Second live req: parsed $res');
  SKIP: {
        skip "Response content did not parse, cannot test", 2
            unless (ref $res and $res->isa('RPC::XML::response'));
        ok($res->is_fault, 'Second live req: parsed $res is a fault');
        like($res->value->value->{faultString}, qr/Unknown method/,
             'Second live request: correct faultString');
    }
}
stop_server($child);

# Start the server again
$child = start_server($srv);
$bucket = 0;
$req->content(RPC::XML::request->new('perl.test.suite.peeraddr')->as_string);
$SIG{ALRM} = sub { $bucket++ };
alarm(120);
$res = $UA->request($req);
alarm(0);
ok(! $bucket, 'Third live-request returned without timeout');
SKIP: {
    skip "Server failed to respond within 120 seconds!", 4 if $bucket;

    ok(! $res->is_error, 'Third live req: Check that $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'Third live req: parsed $res');
  SKIP: {
        skip "Response content did not parse, cannot test", 3
            unless (ref $res and $res->isa('RPC::XML::response'));
        $res = $res->value->value;
        is($res->[2], inet_ntoa(inet_aton('localhost')),
           'Third live req: Correct IP addr from peerhost');
        is($res->[0], inet_aton($res->[2]),
           'Third request: peeraddr packet matches converted peerhost');
        is($res->[1], pack_sockaddr_in($res->[3], inet_aton($res->[2])),
           'Third request: pack_sockaddr_in validates all');
    }
}
stop_server($child);

# Start the server again
# Add a method that echoes back info from the HTTP request object
$res = $srv->add_method({ name      => 'perl.test.suite.http_request',
                          signature => [ 'array' ],
                          code      =>
                          sub {
                              my $srv = shift;

                              [ $srv->{request}->content_type,
                                $srv->{request}->header('X-Foobar') ]
                          } });
$child = start_server($srv);
$bucket = 0;
$req->content(RPC::XML::request->new('perl.test.suite.http_request')->as_string);
$req->header('X-Foobar', 'Wibble');
$SIG{ALRM} = sub { $bucket++ };
alarm(120);
$res = $UA->request($req);
alarm(0);
ok(! $bucket, 'Fourth live-request returned without timeout');
SKIP: {
    skip "Server failed to respond within 120 seconds!", 4 if $bucket;

    ok(! $res->is_error, 'Fourth live req: Check that $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'Fourth live req: parsed $res');
  SKIP: {
        skip "Response content did not parse, cannot test", 3
            unless (ref $res and $res->isa('RPC::XML::response'));
        $res = $res->value->value;
        is($res->[0], 'text/xml',
           'Fourth request: Content type returned correctly');
        is($res->[1], 'Wibble',
           'Fourth live req: Correct value for request header X-Foobar');
    }
}
# Clean up after ourselves.
$req->remove_header('X-Foobar');
stop_server($child);

# Start the server again
$child = start_server($srv);

# Test the error-message-mixup problem reported in RT# 29351
# (http://rt.cpan.org/Ticket/Display.html?id=29351)
my $tmp = q{<?xml version="1.0" encoding="us-ascii"?>
<methodCall>
  <methodName>test.method</methodName>
  <params>
    <param>
      <value><string>foo</string></value>
      <value><string>bar</string></value>
    </param>
  </params>
</methodCall>};
$req->content($tmp);
$bucket = 0;
$SIG{ALRM} = sub { $bucket++ };
alarm(120);
$res = $UA->request($req);
alarm(0);
ok(! $bucket, 'RT29351 live-request returned without timeout');
SKIP: {
    skip "Server failed to respond within 120 seconds!", 4 if $bucket;

    ok(! $res->is_error, 'RT29351 live req: $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'RT29351 live req: parsed $res');
  SKIP: {
        skip "Response content did not parse, cannot test", 2
            unless (ref $res and $res->isa('RPC::XML::response'));
        ok($res->is_fault, 'RT29351 live req: parsed $res is a fault');
        like($res->value->value->{faultString}, qr/Stack corruption/,
             'RT29351 live request: correct faultString');
    }
}
stop_server($child);

# OK-- At this point, basic server creation and accessors have been validated.
# We've run a remote method and we've correctly failed to run an unknown remote
# method. Before moving into the more esoteric XPL-file testing, we will test
# the provided introspection API.
undef $srv;
undef $req;
die "No usable port found between 9000 and 10000, skipping"
    if (($port = find_port) == -1);
$srv = RPC::XML::Server->new(host => 'localhost', port => $port);

# Did it create OK, with the requirement of loading the XPL code?
isa_ok($srv, 'RPC::XML::Server', '$srv<3> (with default methods)');
SKIP: {
    skip "Failed to create RPC::XML::Server object with default methods", 3
        unless ref($srv);

    # Did it get all of them?
    is($srv->list_methods(), scalar(@API_METHODS),
       'Correct number of methods (defaults)');
    $req = HTTP::Request->new(POST => "http://localhost:$port/");

    $child = start_server($srv);

    $req->header(Content_Type => 'text/xml');
    $req->content(RPC::XML::request->new('system.listMethods')->as_string);
    # Use alarm() to manage a reasonable time-out on the request
    $bucket = 0;
    undef $res;
    $SIG{ALRM} = sub { $bucket++ };
    alarm(120);
    $res = $UA->request($req);
    alarm(0);
  SKIP: {
        skip "Server failed to respond within 120 seconds!", 2 if $bucket;

        $res = ($res->is_error) ? '' : $parser->parse($res->content);
        isa_ok($res, 'RPC::XML::response', 'system.listMethods response');
      SKIP: {
            skip "Response content did not parse, cannot test", 1
                unless (ref $res and $res->isa('RPC::XML::response'));
            $list = (ref $res) ? $res->value->value : [];
            ok((ref($list) eq 'ARRAY') &&
               (join('', sort @$list) eq join('', sort @API_METHODS)),
               'system.listMethods return list correct');
        }
    }
}

# Assume $srv is defined, for the rest of the tests (so as to avoid the
# annoying 'ok(0)' streams like above).
die "Server allocation failed, cannot continue. Message was: $srv"
    unless (ref $srv);

stop_server($child);

# Start the server again
$child = start_server($srv);

# Set the ALRM handler to something more serious, since we have passed that
# hurdle already.
$SIG{ALRM} = sub { die "Server failed to respond within 120 seconds\n"; };

# Test the substring-parameter calling of system.listMethods
$req->content(RPC::XML::request->new('system.listMethods',
                                     'method')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    $list = $res->value->value;
    if ($res->is_fault)
    {
        fail(failmsg($res->value->string, __LINE__));
    }
    else
    {
        is(join(',', sort @$list),
           'system.methodHelp,system.methodSignature',
           'system.listMethods("method") return list correct');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# Run again, with a pattern that will produce no matches
$req->content(RPC::XML::request->new('system.listMethods',
                                     'nomatch')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    $list = $res->value->value;
    if ($res->is_fault)
    {
        fail(failmsg($res->value->string, __LINE__));
    }
    else
    {
        is(scalar(@$list), 0,
           'system.listMethods("nomatch") return list correct');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.identity
$req->content(RPC::XML::request->new('system.identity')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    is($res->value->value, $srv->product_tokens, 'system.identity test');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.status
$req->content(RPC::XML::request->new('system.status')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 2 unless $res;
    $res = $res->value->value;
    @keys = qw(host port name version path date date_int started started_int
               total_requests methods_known);
    is(scalar(grep(defined $res->{$_}, @keys)), @keys,
       'system.status hash has correct keys');
    is($res->{total_requests}, 4,
       'system.status reports correct total_requests');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# Test again, with a 'true' value passed to the method, which should prevent
# the 'total_requests' key from incrementing.
$req->content(RPC::XML::request->new('system.status',
                                     RPC::XML::boolean->new(1))->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    $res = $res->value->value;
    is($res->{total_requests}, 4,
       'system.status reports correct total_requests ("true" call)');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.methodHelp
$req->content(RPC::XML::request->new('system.methodHelp',
                                     'system.identity')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    $meth = $srv->get_method('system.identity');
    if (! blessed $meth)
    {
        fail(failmsg($meth, __LINE__));
    }
    else
    {
        is($res->value->value, $meth->{help},
           'system.methodHelp("system.identity") test');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.methodHelp with multiple arguments
$req->content(RPC::XML::request->new('system.methodHelp',
                                     [ 'system.identity',
                                       'system.status' ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    if ($res->is_fault)
    {
        fail(failmsg($res->value->string, __LINE__));
    }
    else
    {
        is(join('', @{ ref($res) ? $res->value->value : [] }),
           $srv->get_method('system.identity')->{help} .
           $srv->get_method('system.status')->{help},
           'system.methodHelp("system.identity", "system.status") test');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.methodHelp with an invalid argument
$req->content(RPC::XML::request->new('system.methodHelp',
                                     'system.bad')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 2 unless $res;
    ok($res->value->is_fault(),
       'system.methodHelp returned fault for unknown method');
    like($res->value->string, qr/Method.*unknown/,
         'system.methodHelp("system.bad") correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.methodSignature
$req->content(RPC::XML::request->new('system.methodSignature',
                                     'system.methodHelp')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    $meth = $srv->get_method('system.methodHelp');
    if (! blessed $meth)
    {
        fail(failmsg($meth, __LINE__));
    }
    else
    {
        is(join('',
                sort map { join(' ', @$_) }
                @{ ref($res) ? $res->value->value : [] }),
           join('', sort @{ $meth->{signature} }),
           'system.methodSignature("system.methodHelp") test');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.methodSignature, with an invalid request
$req->content(RPC::XML::request->new('system.methodSignature',
                                     'system.bad')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 2 unless $res;
    ok($res->value->is_fault(),
       'system.methodSignature returned fault for unknown method');
    like($res->value->string, qr/Method.*unknown/,
         'system.methodSignature("system.bad") correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.introspection
$req->content(RPC::XML::request->new('system.introspection')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    if ($res->is_fault)
    {
        fail(failmsg($res->value->string, __LINE__));
    }
    else
    {
        $list = $res->value->value;
        $bucket = 0;
        %seen = ();
        for $res (@$list)
        {
            if ($seen{$res->{name}}++)
            {
                # If we somehow get the same name twice, that is a point off
                $bucket++;
                next;
            }

            $meth = $srv->get_method($res->{name});
            if ($meth)
            {
                $bucket++ unless
                    (($meth->{help} eq $res->{help}) &&
                     ($meth->{version} eq $res->{version}) &&
                     (join('', sort @{ $res->{signature } }) eq
                      join('', sort @{ $meth->{signature} })));
            }
            else
            {
                # That is also a point
                $bucket++;
            }
        }
        ok(! $bucket, 'system.introspection passed with no errors');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.multicall
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.listMethods',
                                         params => [ 'intro' ] }
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 2 unless $res;
    if ($res->is_fault)
    {
        fail(failmsg($res->value->string, __LINE__));
        fail(failmsg($res->value->string, __LINE__));
    }
    else
    {
        $res = $res->value->value;
        is($res->[0], $srv->product_tokens,
           'system.multicall response elt [0] is correct');
        is((ref($res->[1]) eq 'ARRAY' ? $res->[1]->[0] : ''),
           'system.introspection',
           'system.multicall response elt [1][0] is correct');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.multicall, with an attempt at illegal recursion
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.multicall',
                                         params => [ 'intro' ] }
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 2 unless $res;
    $res = $res->value;
    ok($res->is_fault,
       'system.multicall returned fault on attempt at recursion');
    like($res->string, qr/Recursive/,
         'system.multicall recursion attempt set correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.multicall, with bad data on one of the call specifications
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.status',
                                         params => 'intro' }
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 2 unless $res;
    $res = $res->value;
    ok($res->is_fault,
       'system.multicall returned fault when passed a bad param array');
    like($res->string, qr/value for.*params.*not an array/i,
         'system.multicall bad param array set correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.multicall, with bad data in the request itself
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       'This is not acceptable data'
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 2 unless $res;
    $res = $res->value;
    ok($res->is_fault, 'system.multicall returned fault on bad input');
    like($res->string, qr/one.*array element.*not a struct/i,
         'system.multicall bad input set correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
unless ($res) # $res was made null above if it was an error
{
    stop_server($child);

    # Start the server again
    $child = start_server($srv);
}

# system.status, once more, to check the total_requests value
$req->content(RPC::XML::request->new('system.status')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
SKIP: {
    skip "Server response was error, cannot test", 1 unless $res;
    $res = $res->value->value;
    is($res->{total_requests}, 20, 'system.status, final request tally');
}

# Don't leave any children laying around
stop_server($child);
exit;
