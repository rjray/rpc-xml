#!/usr/bin/perl

# Test the RPC::XML::Server class

use strict;
use subs qw(start_server find_port);
use vars qw($srv $res $bucket $child $parser $xml $req $port $UA @API_METHODS
            $list $meth @keys %seen $dir);

use File::Spec;
use Test;

use LWP::UserAgent;
use HTTP::Request;

require RPC::XML::Server;
require RPC::XML::Parser;

BEGIN { plan tests => 40 }

@API_METHODS = qw(system.identity system.introspection system.listMethods
                  system.methodHelp system.methodSignature system.multicall
                  system.status);

(undef, $dir, undef) = File::Spec->splitpath($0);
require File::Spec->catfile($dir, 'util.pl');

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
$child = start_server($srv);

$req->header(Content_Type => 'text/xml');
$req->content(RPC::XML::request->new('perl.test.suite.test1')->as_string);
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
sleep 1; # To allow the old sockets time enough to go away
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
    ok(ref($res) and $res->is_fault);
    ok(ref($res) and ($res->value->value->{faultString} =~ /Unknown method/));
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
        $list = (ref $res) ? $res->value->value : [];
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

# Assume $srv is defined, for the rest of the tests (so as to avoid the
# annoying 'ok(0)' streams like above).
exit unless (ref $srv);

# Set the ALRM handler to something more serious, since we've passed that
# hurdle already.
$SIG{ALRM} = sub { die "Server failed to respond within 120 seconds\n"; };

#
# Test the substring-parameter calling of system.listMethods
#
$req->content(RPC::XML::request->new('system.listMethods',
                                     'method')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$list = (ref $res) ? $res->value->value : [];
ok((ref($list) eq 'ARRAY') &&
   (join(',', sort @$list) eq 'system.methodHelp,system.methodSignature'));

#
# Again, with a pattern that will produce no matches
#
$req->content(RPC::XML::request->new('system.listMethods',
                                     'microsquirt')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$list = (ref $res) ? $res->value->value : [];
ok((ref($list) eq 'ARRAY') && (@$list == 0));

#
# system.identity
#
$req->content(RPC::XML::request->new('system.identity')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
ok($res->value->value() eq $srv->product_tokens);

#
# system.status
#
$req->content(RPC::XML::request->new('system.status')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$res = $res->value->value;
@keys = qw(host port name version path date date_int started started_int
           total_requests methods_known);
ok((ref($res) eq 'HASH') && (grep(defined $res->{$_}, @keys) == @keys) &&
   ($res->{total_requests} == 5));

#
# system.methodHelp
#
$req->content(RPC::XML::request->new('system.methodHelp',
                                     'system.identity')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$meth = $srv->get_method('system.identity');
ok($res->value->value() eq $meth->{help});

#
# system.methodHelp with multiple arguments
#
$req->content(RPC::XML::request->new('system.methodHelp',
                                     [ 'system.identity',
                                       'system.status' ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
ok(join('', @{ $res->value->value }) eq
   $srv->get_method('system.identity')->{help} .
   $srv->get_method('system.status')->{help});

#
# system.methodHelp with an invalid argument
#
$req->content(RPC::XML::request->new('system.methodHelp',
                                     'system.teaseMe')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
ok(ref($res) && $res->value->is_fault() &&
   $res->value->string() =~ /Method.*unknown/);

#
# system.methodSignature
#
$req->content(RPC::XML::request->new('system.methodSignature',
                                     'system.methodHelp')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$meth = $srv->get_method('system.methodHelp');
ok(join('', sort (map { join(' ', @$_) } @{ $res->value->value })) eq
   join('', sort @{ $meth->{signature} }));

#
# system.methodSignature, with an invalid request
#
$req->content(RPC::XML::request->new('system.methodSignature',
                                     'system.teaseMe')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
ok(ref($res) && $res->value->is_fault() &&
   $res->value->string() =~ /Method.*unknown/);

#
# system.introspection
#
$req->content(RPC::XML::request->new('system.introspection')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$list = (ref $res) ? $res->value->value : [];
$bucket = 0;
%seen = ();
for $res (@$list)
{
    if ($seen{$res->{name}}++)
    {
        # If we somehow get the same name twice, that's a point off
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
        # That's a point
        $bucket++;
    }
}
ok(! $bucket);

#
# system.multicall
#
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.listMethods',
                                         params => [ 'intro' ] }
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$res = $res->value->value;
ok((ref($res) eq 'ARRAY') && ($res->[0] eq $srv->product_tokens) &&
   ($res->[1]->[0] eq 'system.introspection'));

#
# system.multicall, with an attempt at illegal recursion
#
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.multicall',
                                         params => [ 'intro' ] }
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$res = $res->value;
ok($res->is_fault && $res->string =~ /Recursive/);

#
# system.multicall, with bad data on one of the call specifications
#
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       { methodName => 'system.status',
                                         params => 'intro' }
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$res = $res->value;
ok($res->is_fault && $res->string =~ /value for.*params.*not an array/i);

#
# system.multicall, with bad data in the request itself
#
$req->content(RPC::XML::request->new('system.multicall',
                                     [ { methodName => 'system.identity' },
                                       'This is not acceptable data'
                                     ])->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$res = $res->value;
ok($res->is_fault && $res->string =~ /one.*array element.*not a struct/i);

#
# system.status, once more, to check the total_requests value
#
$req->content(RPC::XML::request->new('system.status')->as_string);
alarm(120);
$res = $UA->request($req);
alarm(0);
$res = ($res->is_error) ? '' : $parser->parse($res->content);
$res = $res->value->value;
ok($res->{total_requests} == 21);

# Don't leave any children laying around
kill 'INT', $child;
exit;
