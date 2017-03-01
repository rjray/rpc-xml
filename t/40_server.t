#!/usr/bin/env perl

# Test the RPC::XML::Server class

## no critic(RequireInterpolationOfMetachars)

use strict;
use warnings;
use subs qw(start_server stop_server find_port_in_use);

use Carp qw(croak);
use IO::Socket;
use File::Spec;
use List::Util 'none';
use Scalar::Util 'blessed';
use Socket ();

use Test::More;
use LWP::UserAgent;
use HTTP::Request;

use RPC::XML 'RPC_BASE64';
use RPC::XML::Server;
use RPC::XML::ParserFactory;

plan tests => 90;

my ($srv, $res, $bucket, $child, $parser, $xml, $req, $port, $UA, @API_METHODS,
    $list, $meth, %seen, $dir, $vol, $oldtable, $newtable, $value);

@API_METHODS = qw(system.identity system.introspection system.listMethods
                  system.methodHelp system.methodSignature system.multicall
                  system.status);

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, q{});
require File::Spec->catfile($dir, 'util.pl');

sub failmsg {
    my ($msg, $line) = @_;

    return sprintf '%s at line %d', $msg, $line;
}

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# any other classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Server. In this particular case, this
# also means that we cannot use RPC::XML::Client to test it.

# Start with some very basic things, without actually firing up a live server.
$srv = RPC::XML::Server->new(no_http => 1, no_default => 1);
isa_ok($srv, 'RPC::XML::Server', '$srv<1>');

# This assignment is just to suppress "used only once" warnings
$value = $RPC::XML::Server::VERSION;
is($srv->version, $RPC::XML::Server::VERSION,
   'RPC::XML::Server::version method');
ok(! $srv->started, 'RPC::XML::Server::started method');
like($srv->product_tokens, qr{/}, 'RPC::XML::Server::product_tokens method');
ok(! $srv->url, 'RPC::XML::Server::url method (empty)');
ok(! $srv->requests, 'RPC::XML::Server::requests method (0)');
ok($srv->response->isa('HTTP::Response'),
   'RPC::XML::Server::response method returns HTTP::Response');
# Some negative tests:
$res = $srv->new();
like($res, qr/Must be called as a static method/,
     'Calling new() as an instance method fails');
$meth = $srv->method_from_file('does_not_exist.xpl');
ok(! ref $meth, 'Bad file did not result in method reference');
like($meth, qr/Error opening.*does_not_exist/, 'Correct error message');

# Test the functionality of manipulating the fault table. First get the vanilla
# table from a simple server object. Then create a new server object with both
# a fault-base offset and some user-defined faults. We use the existing $srv to
# get the "plain" table.
$oldtable = $srv->{__fault_table};
# Now re-assign $srv
$srv = RPC::XML::Server->new(
    no_http         => 1,
    no_default      => 1,
    fault_code_base => 1000,
    fault_table     => {
        myfault1 => [ 2000, 'test' ],
        myfault2 => 2001,
    }
);
$newtable = $srv->{__fault_table};
# Compare number of faults, the values of the fault codes, and the presence of
# the user-defined faults:
ok((scalar(keys %{$oldtable}) + 2) == (scalar keys %{$newtable}),
   'Proper number of relative keys');
$value = 1;
for my $key (keys %{$oldtable}) {
    if ($newtable->{$key}->[0] != ($oldtable->{$key}->[0] + 1000)) {
        $value = 0;
        last;
    }
}
ok($value, 'Fault codes adjustment yielded correct new codes');
ok((exists $newtable->{myfault1} && exists $newtable->{myfault2} &&
    ref($newtable->{myfault1}) eq 'ARRAY' && $newtable->{myfault2} == 2001 &&
    $newtable->{myfault1}->[0] == 2000),
   'User-supplied fault elements look OK');

# Done with this one, let it go
undef $srv;

# Test that the url() method behaves like we expect it for certain ports
$srv = RPC::XML::Server->new(
    no_default => 1,
    no_http    => 1,
    host       => 'localhost',
    port       => 80
);
SKIP: {
    if (ref($srv) ne 'RPC::XML::Server') {
        skip 'Failed to get port-80 server, cannot test', 1;
    }

    is($srv->url, 'http://localhost', 'Default URL for port-80 server');
}

$srv = RPC::XML::Server->new(
    no_default => 1,
    no_http    => 1,
    host       => 'localhost',
    port       => 443
);
SKIP: {
    if (ref($srv) ne 'RPC::XML::Server') {
        skip 'Failed to get port-443 server, cannot test', 1;
    }

    is($srv->url, 'https://localhost', 'Default URL for port-443 server');
}

# Let's test that server creation properly fails if/when HTTP::Daemon fails.
# First find a port in use, preferably under 1024:
SKIP: {
    if ($< == 0) {
        skip 'Negative port-based test unreliable when run as root', 2;
    }
    $port = find_port_in_use;
    if ($port == -1) {
        skip 'No in-use port found for negative testing, skipped', 2;
    }

    $srv = RPC::XML::Server->new(port => $port);
    is(ref($srv), q{}, 'Bad new return is not an object');
    like($srv, qr/Unable to create HTTP::Daemon/, 'Proper error message');
}

# This one will have a HTTP::Daemon server, but still no default methods
$srv = RPC::XML::Server->new(no_default => 1, host => 'localhost');
isa_ok($srv, 'RPC::XML::Server', '$srv<2>');
if (! ref $srv) {
    croak "Server allocation failed, cannot continue. Message was: $srv";
}

# Test some of the simpler cases of add_method and get_method
$res = $srv->add_method({ name      => 'perl.test.suite.test1',
                          signature => [ 'int' ],
                          code      => sub { return 1; } });
ok($res eq $srv, 'add_method return value test');
$res = $srv->get_method('perl.test.suite.test1');
isa_ok($res, 'RPC::XML::Method', 'get_method return value');
$res = $srv->get_method('perl.test.suite.not.added.yet');
ok(! ref($res), 'get_method for non-existent method');

# Throw junk at add_method/add_procedure/add_function
$res = $srv->add_method([]);
like($res, qr/file name, a hash reference or an object/,
     'add_method() fails on bad data');
$res = $srv->add_method('file does not exist');
like($res, qr/Error loading from file/,
     'add_method() fails on non-existent file');
$res = $srv->add_procedure({ name      => 'procedure1',
                             signature => [ 'int' ],
                             code      => sub { return 1; } });
ok($res eq $srv, 'add_procedure return value test');
$res = $srv->get_procedure('procedure1');
is(ref($res), 'RPC::XML::Procedure', 'get_procedure(procedure1) return value');
$res = $srv->add_function({ name => 'function1',
                            code => sub { return 1; } });
ok($res eq $srv, 'add_function return value test');
$res = $srv->get_function('function1');
is(ref($res), 'RPC::XML::Function', 'get_function(function1) return value');
$res = $srv->add_method({ name      => 'method1',
                          type      => 'bad',
                          signature => [ 'int' ],
                          code      => sub { return 1; } });
like($res, qr/Unknown type: bad/, 'add_method, bad type param');

# Here goes...
$parser = RPC::XML::ParserFactory->new;
$UA = LWP::UserAgent->new;
$req = HTTP::Request->new(POST => $srv->url);
$child = start_server $srv;

$req->header(Content_Type => 'text/xml');
$req->content(RPC::XML::request->new('perl.test.suite.test1')->as_string);
# Use alarm() to manage a resaonable time-out on the request
$bucket = 0;
local $SIG{ALRM} = sub { $bucket++ };
alarm 120;
$res = $UA->request($req);
alarm 0;
ok(! $bucket, 'First live-request returned without timeout');
SKIP: {
    if ($bucket)
    {
        skip 'Server failed to respond within 120 seconds!', 4;
    }

    ok(! $res->is_error, 'First live req: Check that $res is not an error');
    $xml = $res->content;
    $res = $parser->parse($xml);
    isa_ok($res, 'RPC::XML::response', 'First live req: parsed $res');
  SKIP: {
        if (! (ref $res and $res->isa('RPC::XML::response')))
        {
            skip 'Response content did not parse, cannot test', 2;
        }

        ok(! $res->is_fault, 'First live req: parsed $res is not a fault');
        is($res->value->value, 1, 'First live req: $res value test');
    }
}
stop_server $child;

# Try deleting the method
ok(ref $srv->delete_method('perl.test.suite.test1'),
   'delete_method return value test');

# Start the server again
# Add a method that echoes back socket-peer information
$res = $srv->add_method({ name      => 'perl.test.suite.peeraddr',
                          signature => [ 'array' ],
                          code      =>
                          sub {
                              my $server = shift;

                              my $peerfamily = RPC_BASE64 $server->{peerfamily};
                              my $peeraddr = RPC_BASE64 $server->{peeraddr};
                              my $packet = pack_sockaddr_any(
                                  $server->{peerfamily},
                                  $server->{peerhost},
                                  $server->{peerport}
                              );
                              $packet = RPC_BASE64 $packet;

                              [ $peerfamily, $peeraddr, $packet,
                                $server->{peerhost}, $server->{peerport} ];
                          } });
$child = start_server $srv;
$bucket = 0;
alarm 120;
$res = $UA->request($req);
alarm 0;
ok(! $bucket, 'Second live-request returned without timeout');
SKIP: {
    if ($bucket)
    {
        skip 'Server failed to respond within 120 seconds!', 4;
    }

    ok(! $res->is_error, 'Second live req: Check that $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'Second live req: parsed $res');
  SKIP: {
        if (! (ref $res and $res->isa('RPC::XML::response')))
        {
            skip 'Response content did not parse, cannot test', 2;
        }

        ok($res->is_fault, 'Second live req: parsed $res is a fault');
        like($res->value->value->{faultString}, qr/Unknown method/,
             'Second live request: correct faultString');
    }
}
stop_server $child;

# Start the server again
$child = start_server $srv;
$bucket = 0;
$req->content(RPC::XML::request->new('perl.test.suite.peeraddr')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
ok(! $bucket, 'Third live-request returned without timeout');
SKIP: {
    if ($bucket)
    {
        skip 'Server failed to respond within 120 seconds!', 4;
    }

    ok(! $res->is_error, 'Third live req: Check that $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'Third live req: parsed $res');
  SKIP: {
        if (! (ref $res and $res->isa('RPC::XML::response')))
        {
            skip 'Response content did not parse, cannot test', 3;
        }

        $res = $res->value->value;
        ok(grep({ $_ eq $res->[3]} resolve($res->[0], 'localhost')),
           'Third live req: Correct IP addr from peerhost');
        is($res->[1], Socket::inet_pton($res->[0], $res->[3]),
           'Third request: peeraddr packet matches converted peerhost');
        is($res->[2], pack_sockaddr_any($res->[0], $res->[3], $res->[4]),
           'Third request: pack_sockaddr_any validates all');
    }
}
stop_server $child;

# Start the server again
# Add a method that echoes back info from the HTTP request object
$res = $srv->add_method({ name      => 'perl.test.suite.http_req',
                          signature => [ 'array' ],
                          code      =>
                          sub {
                              my $server = shift;

                              [ $server->{request}->content_type,
                                $server->{request}->header('X-Foobar') ]
                          } });
$child = start_server $srv;
$bucket = 0;
$req->content(RPC::XML::request->new('perl.test.suite.http_req')->as_string);
$req->header('X-Foobar', 'Wibble');
alarm 120;
$res = $UA->request($req);
alarm 0;
ok(! $bucket, 'Fourth live-request returned without timeout');
SKIP: {
    if ($bucket)
    {
        skip 'Server failed to respond within 120 seconds!', 4;
    }

    ok(! $res->is_error, 'Fourth live req: Check that $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'Fourth live req: parsed $res');
  SKIP: {
        if (! (ref $res and $res->isa('RPC::XML::response')))
        {
            skip 'Response content did not parse, cannot test', 2;
        }

        $res = $res->value->value;
        is($res->[0], 'text/xml',
           'Fourth request: Content type returned correctly');
        is($res->[1], 'Wibble',
           'Fourth live req: Correct value for request header X-Foobar');
    }
}
# Clean up after ourselves.
$req->remove_header('X-Foobar');
stop_server $child;

# Start the server again
$child = start_server $srv;

# Test the error-message-mixup problem reported in RT# 29351
# (http://rt.cpan.org/Ticket/Display.html?id=29351)
my $tmp = <<'EOX';
<?xml version="1.0" encoding="us-ascii"?>
<methodCall>
  <methodName>test.method</methodName>
  <params>
    <param>
      <value><string>foo</string></value>
      <value><string>bar</string></value>
    </param>
  </params>
</methodCall>
EOX
$req->content($tmp);
$bucket = 0;
alarm 120;
$res = $UA->request($req);
alarm 0;
ok(! $bucket, 'RT29351 live-request returned without timeout');
SKIP: {
    if ($bucket) {
        skip 'Server failed to respond within 120 seconds!', 4;
    }

    ok(! $res->is_error, 'RT29351 live req: $res is not an error');
    $res = $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'RT29351 live req: parsed $res');
  SKIP: {
        if (! (ref $res and $res->isa('RPC::XML::response'))) {
            skip 'Response content did not parse, cannot test', 2;
        }

        ok($res->is_fault, 'RT29351 live req: parsed $res is a fault');
        like(
            $res->value->value->{faultString},
            qr/Illegal content in param tag/,
            'RT29351 live request: correct faultString'
        );
    }
}
stop_server $child;

# OK-- At this point, basic server creation and accessors have been validated.
# We've run a remote method and we've correctly failed to run an unknown remote
# method. Before moving into the more esoteric XPL-file testing, we will test
# the provided introspection API.
undef $srv;
undef $req;
$srv = RPC::XML::Server->new(host => 'localhost');

# Did it create OK, with the requirement of loading the XPL code?
isa_ok($srv, 'RPC::XML::Server', '$srv<3> (with default methods)');
# Assume $srv is defined for the rest of the tests
if (! ref $srv) {
    croak "Server allocation failed, cannot continue. Message was: $srv";
}

# Did it get all of them?
is($srv->list_methods(), scalar(@API_METHODS),
   'Correct number of methods (defaults)');
$req = HTTP::Request->new(POST => $srv->url);

$child = start_server $srv;

$req->header(Content_Type => 'text/xml');
$req->content(RPC::XML::request->new('system.listMethods')->as_string);
# Use alarm() to manage a reasonable time-out on the request
$bucket = 0;
undef $res;
alarm 120;
$res = $UA->request($req);
alarm 0;
SKIP: {
    if ($bucket) {
        skip 'Server failed to respond within 120 seconds!', 2;
    }

    $res = ($res->is_error) ? q{} : $parser->parse($res->content);
    isa_ok($res, 'RPC::XML::response', 'system.listMethods response');
  SKIP: {
        if (! (ref $res and $res->isa('RPC::XML::response'))) {
            skip 'Response content did not parse, cannot test', 1;
        }

        $list = (ref $res) ? $res->value->value : [];
        ok((ref($list) eq 'ARRAY') &&
               (join(q{} => sort @{$list}) eq join q{} => sort @API_METHODS),
           'system.listMethods return list correct');
    }
}

stop_server $child;

# Start the server again
$child = start_server $srv;

# Set the ALRM handler to something more serious, since we have passed that
# hurdle already.
local $SIG{ALRM} = sub { die "Server failed to respond within 120 seconds\n"; };

# Test the substring-parameter calling of system.listMethods
$req->content(RPC::XML::request->new('system.listMethods',
                                     'method')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    $list = $res->value->value;
    if ($res->is_fault) {
        fail(failmsg($res->value->string, __LINE__));
    } else {
        is(join(q{,} => sort @{$list}),
           'system.methodHelp,system.methodSignature',
           'system.listMethods("method") return list correct');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# Run again, with a pattern that will produce no matches
$req->content(RPC::XML::request->new('system.listMethods',
                                     'nomatch')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    $list = $res->value->value;
    if ($res->is_fault) {
        fail(failmsg($res->value->string, __LINE__));
    } else {
        is(scalar(@{$list}), 0,
           'system.listMethods("nomatch") return list correct');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.identity
$req->content(RPC::XML::request->new('system.identity')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    is($res->value->value, $srv->product_tokens, 'system.identity test');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.status
$req->content(RPC::XML::request->new('system.status')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 2;
    }

    $res = $res->value->value;
    my @keys = qw(host port name version path date date_int started started_int
                  total_requests methods_known);
    my @seen_keys = grep { defined $res->{$_} } @keys;
    ok(@keys == @seen_keys, 'system.status hash has correct keys');
    is($res->{total_requests}, 4,
       'system.status reports correct total_requests');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# Test again, with a 'true' value passed to the method, which should prevent
# the 'total_requests' key from incrementing.
$req->content(RPC::XML::request->new('system.status',
                                     RPC::XML::boolean->new(1))->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    $res = $res->value->value;
    is($res->{total_requests}, 4,
       'system.status reports correct total_requests ("true" call)');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.methodHelp
$req->content(RPC::XML::request->new('system.methodHelp',
                                     'system.identity')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    $meth = $srv->get_method('system.identity');
    if (! blessed $meth) {
        fail(failmsg($meth, __LINE__));
    } else {
        is($res->value->value, $meth->{help},
           'system.methodHelp("system.identity") test');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.methodHelp with multiple arguments
$req->content(RPC::XML::request->new('system.methodHelp',
                                     [ 'system.identity',
                                       'system.status' ])->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    if ($res->is_fault) {
        fail(failmsg($res->value->string, __LINE__));
    } else {
        is(join(q{}, @{ ref($res) ? $res->value->value : [] }),
           $srv->get_method('system.identity')->{help} .
           $srv->get_method('system.status')->{help},
           'system.methodHelp("system.identity", "system.status") test');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.methodHelp with an invalid argument
$req->content(RPC::XML::request->new('system.methodHelp',
                                     'system.bad')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 2;
    }

    ok($res->value->is_fault(),
       'system.methodHelp returned fault for unknown method');
    like($res->value->string, qr/Method.*unknown/,
         'system.methodHelp("system.bad") correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.methodSignature
$req->content(RPC::XML::request->new('system.methodSignature',
                                     'system.methodHelp')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    $meth = $srv->get_method('system.methodHelp');
    if (! blessed $meth) {
        fail(failmsg($meth, __LINE__));
    } else {
        is(join(q{},
                sort map { join q{ } => @{$_} }
                @{ ref($res) ? $res->value->value : [] }),
           join(q{} => sort @{$meth->{signature}}),
           'system.methodSignature("system.methodHelp") test');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.methodSignature, with an invalid request
$req->content(RPC::XML::request->new('system.methodSignature',
                                     'system.bad')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 2;
    }

    ok($res->value->is_fault(),
       'system.methodSignature returned fault for unknown method');
    like($res->value->string, qr/Method.*unknown/,
         'system.methodSignature("system.bad") correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.introspection
$req->content(RPC::XML::request->new('system.introspection')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    if ($res->is_fault) {
        fail(failmsg($res->value->string, __LINE__));
    } else {
        $list = $res->value->value;
        $bucket = 0;
        %seen = ();
        for my $result (@{$list}) {
            if ($seen{$result->{name}}++) {
                # If we somehow get the same name twice, that is a point off
                $bucket++;
                next;
            }

            $meth = $srv->get_method($result->{name});
            if ($meth) {
                my $result_sig = join q{} => sort @{$result->{signature}};
                my $method_sig = join q{} => sort @{$meth->{signature}};
                # A point off unless all three of these match
                if (($meth->{help} ne $result->{help}) ||
                        ($meth->{version} ne $result->{version}) ||
                        ($result_sig ne $method_sig)) {
                    $bucket++;
                }
            } else {
                # That is also a point
                $bucket++;
            }
        }
        ok(! $bucket, 'system.introspection passed with no errors');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.multicall
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.listMethods',
                                         params => [ 'intro' ] }
                                     ])->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 2;
    }

    if ($res->is_fault) {
        fail(failmsg($res->value->string, __LINE__));
        fail(failmsg($res->value->string, __LINE__));
    } else {
        $res = $res->value->value;
        is($res->[0], $srv->product_tokens,
           'system.multicall response elt [0] is correct');
        is((ref($res->[1]) eq 'ARRAY' ? $res->[1]->[0] : q{}),
           'system.introspection',
           'system.multicall response elt [1][0] is correct');
    }
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.multicall, with an attempt at illegal recursion
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.multicall',
                                         params => [ 'intro' ] }
                                     ])->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 2;
    }

    $res = $res->value;
    ok($res->is_fault,
       'system.multicall returned fault on attempt at recursion');
    like($res->string, qr/Recursive/,
         'system.multicall recursion attempt set correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.multicall, with bad data on one of the call specifications
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.status',
                                         params => 'intro' }
                                     ])->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 2;
    }

    $res = $res->value;
    ok($res->is_fault,
       'system.multicall returned fault when passed a bad param array');
    like($res->string, qr/value for.*params.*not an array/i,
         'system.multicall bad param array set correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.multicall, with bad data in the request itself
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       'This is not acceptable data'
                                     ])->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 2;
    }

    $res = $res->value;
    ok($res->is_fault, 'system.multicall returned fault on bad input');
    like($res->string, qr/one.*array element.*not a struct/i,
         'system.multicall bad input set correct faultString');
}

# If the response was any kind of error, kill and re-start the server, as
# HTTP::Message::content might have killed it already via croak().
if (! $res) {
    # $res was made null above if it was an error
    stop_server $child;

    # Start the server again
    $child = start_server $srv;
}

# system.status, once more, to check the total_requests value
$req->content(RPC::XML::request->new('system.status')->as_string);
alarm 120;
$res = $UA->request($req);
alarm 0;
$res = ($res->is_error) ? q{} : $parser->parse($res->content);
SKIP: {
    if (! $res) {
        skip 'Server response was error, cannot test', 1;
    }

    $res = $res->value->value;
    is($res->{total_requests}, 20, 'system.status, final request tally');
}

# This time we have to stop the server regardless of whether the response was
# an error. We're going to add some more methods to test some of the error code
# and other bits in RPC::XML::Procedure.
stop_server $child;
$srv->add_method({
    type      => 'procedure',
    name      => 'argcount.p',
    signature => [ 'int' ],
    code      => sub { return scalar @_; },
});
$srv->add_method({
    name      => 'argcount.m',
    signature => [ 'int' ],
    code      => sub { return scalar @_; },
});
$srv->add_method({
    type => 'function',
    name => 'argcount.f',
    code => sub { return scalar @_; },
});
$srv->add_method({
    name      => 'die1',
    signature => [ 'int' ],
    code      => sub { die "die\n"; },
});
{
    ## no critic(RequireCarping)
    $srv->add_method({
        name      => 'die2',
        signature => [ 'int' ],
        code      => sub { die RPC::XML::fault->new(999, 'inner fault'); },
    });
}

# Start the server again, with the new methods
$child = start_server $srv;

# First, call the argcount.? routines, to see that we are getting the correct
# number of args passed in. Up to now, everything running on $srv has been in
# the RPC::XML::Method class. This will test some of the other code.
my @returns = ();
local $SIG{ALRM} = sub { $bucket++ };
for my $type (qw(p m f)) {
    $req->content(RPC::XML::request->new("argcount.$type")->as_string);
    $bucket = 0;
    alarm 120;
    $res = $UA->request($req);
    alarm 0;
    if ($bucket) {
        push @returns, 'timed-out';
    } else {
        $res = $parser->parse($res->content);
        if (ref($res) ne 'RPC::XML::response') {
            push @returns, 'parse-error';
        } else {
            push @returns, $res->value->value;
        }
    }
}
# Finally, test what we got from those three calls:
is(join(q{,} => @returns), '0,1,0', 'Arg-count testing of procedure types');

# While we're at it... test that a ::Function can take any args list
$req->content(RPC::XML::request->new('argcount.f', 1, 1, 1)->as_string);
$bucket = 0;
alarm 120;
$res = $UA->request($req);
alarm 0;
SKIP: {
    if ($bucket) {
        skip 'Second call to argcount.f timed out', 1;
    } else {
        $res = $parser->parse($res->content);
        if (ref($res) ne 'RPC::XML::response') {
            skip 'Second call to argcount.f failed to parse', 1;
        } else {
            is($res->value->value, 3, 'A function takes any argslist');
        }
    }
}

# And test that those that aren't ::Function recognize bad parameter lists
$req->content(RPC::XML::request->new('argcount.p', 1, 1, 1)->as_string);
$bucket = 0;
alarm 120;
$res = $UA->request($req);
alarm 0;
SKIP: {
    if ($bucket) {
        skip 'Second call to argcount.f timed out', 1;
    } else {
        $res = $parser->parse($res->content);
        if (ref($res) ne 'RPC::XML::response') {
            skip 'Second call to argcount.f failed to parse', 1;
        } else {
            if (! $res->is_fault) {
                skip 'Test did not return fault, cannot test', 2;
            }

            is($res->value->code, 201,
               'Bad params list test: Correct faultCode');
            like($res->value->string,
                 qr/no matching signature for the argument list/,
                 'Bad params list test: Correct faultString');
        }
    }
}

# Test behavior when the called function throws an exception
my %die_tests = (
    die1 => {
        code   => 300,
        string => "Code execution error: Method die1 returned error: die\n",
    },
    die2 => {
        code   => 999,
        string => 'inner fault',
    },
);
for my $test (sort keys %die_tests) {
    $req->content(RPC::XML::request->new($test)->as_string);
    $bucket = 0;
    alarm 120;
    $res = $UA->request($req);
    alarm 0;
  SKIP: {
        if ($bucket) {
            skip "Test '$test' timed out, cannot test results", 2;
        } else {
            $res = $parser->parse($res->content);
            if (ref($res) ne 'RPC::XML::response') {
                skip "Test '$test' failed to parse, cannot test results", 2;
            } else {
                if (! $res->is_fault) {
                    skip "Test '$test' did not return fault, cannot test", 2;
                }

                is($res->value->code, $die_tests{$test}{code},
                   "Test $test: Correct faultCode");
                is($res->value->string, $die_tests{$test}{string},
                   "Test $test: Correct faultString");
            }
        }
    }
}

# Don't leave any children laying around
stop_server $child, 'final';

exit;

sub find_port_in_use {
    my $start_at = shift;
    $start_at ||= 80;

    for my $port ($start_at .. ($start_at + 1000)) {
        my $sock = IO::Socket->new(
            Domain   => AF_INET,
            PeerAddr => 'localhost',
            PeerPort => $port
        );
        return $port if ref $sock;
    }

    return -1;
}
