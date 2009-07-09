#!/usr/bin/perl

# Test the RPC::XML::Server class with Net::Server rather than HTTP::Daemon

# This is run after the test suite for RPC::XML::Client, so we will use that
# for the client-side of the tests.

use strict;
use vars qw($dir $srv $pid_file $log_file $port $client $res @keys $meth $list
            $bucket %seen);
use subs qw(start_server find_port);

use File::Spec;
use Test::More;

eval "use Net::Server";
# If they do not have Net::Server, quietly skip
plan skip_all => 'Net::Server not available' if $@;
# ...or if they are on Windows, skip
plan skip_all => 'Net::Server tests not reliable on Windows platform'
    if ($^O eq "MSWin32");
# otherwise...
plan tests => 30;

require RPC::XML::Server;
require RPC::XML::Client;

(undef, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
require File::Spec->catfile($dir, 'util.pl');

$pid_file  = File::Spec->catfile($dir, 'net_server.pid');
$log_file  = File::Spec->catfile($dir, 'net_server.log');
die "No usable port found between 9000 and 10000, skipping"
    if (($port = find_port) == -1);

unlink $log_file if (-e $log_file);
unlink $pid_file if (-e $pid_file);

# All this, and we haven't even created a server object or run a test yet

$srv = RPC::XML::Server->new(no_http => 1);
# Technically, this is overkill. But if it fails everything else blows up:
isa_ok($srv, 'RPC::XML::Server');

# All of these parameters are passed to the run() method of
# Net::Server::MultiType
start_server($srv,
             server_type => 'Single',
             log_file    => $log_file,
             log_level   => 4,
             pid_file    => $pid_file,
             port        => $port,
             host        => 'localhost',
             background  => 1);
sleep 1; # Allow time for server to spin up
# Unless we see "ok 2", we have a problem
ok(-e $pid_file, 'server started, PID file exists');
# After this point, we have the obligation of killing the server manually
$client = RPC::XML::Client->new("http://localhost:$port");
is($client->simple_request('system.identity'), $srv->product_tokens,
   'system.identity matches $srv->product_tokens');

# At this point, most of this is copied from the first server test suite (40).
# We do want to verify the full introspection API under Net::Server, though.

$res = $client->simple_request('system.listMethods');
@keys = $srv->list_methods;
is(ref($res), 'ARRAY', 'system.listMethods returned ARRAY ref');
SKIP: {
    skip 'server response not an ARRAY reference', 2 unless ref($res);

    is(scalar(@$res), scalar(@keys),
       'system.listMethods returned correct number of names');
    is(join('', sort @$res), join('', sort @keys),
       'system.listMethods returned matching set of names');
}

# Test the substring-parameter calling of system.listMethods
$res = $client->simple_request('system.listMethods', 'method');
is(ref($res), 'ARRAY', 'system.listMethods returned ARRAY ref');
SKIP: {
    skip 'server response not an ARRAY reference', 1 unless ref($res);

    is(join(',', sort @$res), 'system.methodHelp,system.methodSignature',
       'system.listMethods with pattern returned correct set of names');
}

# Again, with a pattern that will produce no matches
$res = $client->simple_request('system.listMethods', 'none_will_match');
is(ref($res), 'ARRAY', 'system.listMethods returned ARRAY ref');
SKIP: {
    skip 'server response not an ARRAY reference', 1 unless ref($res);
    is(scalar(@$res), 0, 'system.listMethods with bad pattern returned none');
}

# system.status
$res = $client->simple_request('system.status');
@keys = qw(host port name version path date date_int started started_int
           total_requests methods_known);
is(ref($res), 'HASH', 'system.listMethods returned HASH ref');
SKIP: {
    skip 'server response not a HASH reference', 2 unless ref($res);
    is(scalar(grep(defined $res->{$_}, @keys)), scalar(@keys),
       'system.status hashref has correct number of keys');
    is($res->{total_requests}, 5, 'system.status total_request count correct');
}

# system.methodHelp
$res = $client->simple_request('system.methodHelp', 'system.identity');
is($res, $srv->get_method('system.identity')->{help},
   'system.methodHelp returned correct string');

# system.methodHelp with multiple arguments
$res = $client->simple_request('system.methodHelp',
                             [ 'system.identity', 'system.status' ]);
is(ref($res), 'ARRAY', 'system.methodHelp returned ARRAY ref');
SKIP: {
    skip 'server response not an ARRAY reference', 1 unless ref($res);
    is(join('', @$res),
       $srv->get_method('system.identity')->{help} .
       $srv->get_method('system.status')->{help},
       'system.methodHelp with specific methods returns correctly');
}

# system.methodHelp with an invalid argument
$res = $client->send_request('system.methodHelp', 'system.bad');
isa_ok($res, 'RPC::XML::fault', 'system.methodHelp (bad arg) response');
SKIP: {
    skip 'server response not an RPC::XML data object', 1 unless ref($res);
    like($res->string(), qr/Method.*unknown/,
         'system.methodHelp (bad arg) has correct faultString');
}

# system.methodSignature
$res = $client->simple_request('system.methodSignature', 'system.methodHelp');
is(ref($res), 'ARRAY', 'system.methodHelp returned ARRAY ref');
SKIP: {
    skip 'server response not an ARRAY reference', 1 unless ref($res);
    is(join('', sort (map { join(' ', @$_) } @$res)),
       join('', sort @{ $srv->get_method('system.methodHelp')->{signature} }),
       'system.methodSignature return value correct');
}

# system.methodSignature, with an invalid request
$res = $client->send_request('system.methodSignature', 'system.bad');
isa_ok($res, 'RPC::XML::fault', 'system.methodSignature (bad arg) response');
SKIP: {
    skip 'server response not an RPC::XML data object', 1 unless ref($res);
    like($res->string(), qr/Method.*unknown/,
         'system.methodSignature (bad arg) has correct faultString');
}

# system.introspection
$list = $client->simple_request('system.introspection');
$bucket = 0;
%seen = ();
SKIP: {
    skip 'system.introspection call did not return ARRAY ref', 1
        unless (ref($list) eq 'ARRAY');

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
    ok(! $bucket, 'system.introspection return data is correct');
}

# system.multicall
$res = $client->simple_request('system.multicall',
                               [ { methodName => 'system.identity' },
                                 { methodName => 'system.listMethods',
                                   params => [ 'intro' ] } ]);
is(ref($res), 'ARRAY', 'system.methodHelp returned ARRAY ref');
SKIP: {
    skip 'server response not an ARRAY reference', 2 unless ref($res);
    is($res->[0], $srv->product_tokens,
       'system.multicall, first return value correct');
  SKIP: {
        skip 'system.multicall return value second index not ARRAY ref', 1
            unless (ref($res->[1]) eq 'ARRAY');
        is(scalar(@{$res->[1]}), 1,
           'system.multicall, second return value correct length');
        is($res->[1]->[0], 'system.introspection',
           'system.multicall, second return value correct value');
    }
}

# system.multicall, with an attempt at illegal recursion
$res = $client->send_request('system.multicall',
                             [ { methodName => 'system.identity' },
                               { methodName => 'system.multicall',
                                 params => [ 'intro' ] } ]);
SKIP: {
    skip 'system.multicall (recursion) response error, cannot test', 1 unless
        (ref($res) eq 'RPC::XML::fault');
    like($res->string, qr/Recursive/,
         'system.multicall recursion attempt set correct faultString');
}

# system.multicall, with bad data on one of the call specifications
$res = $client->send_request('system.multicall',
                             [ { methodName => 'system.identity' },
                               { methodName => 'system.listMethods',
                                 params => 'intro' } ]);
SKIP: {
    skip 'system.multicall (bad data) response error, cannot test', 1 unless
        (ref($res) eq 'RPC::XML::fault');
    like($res->string, qr/value for.*params.*not an array/i,
         'system.multicall bad param array set correct faultString');
}

# system.status, once more, to check the total_requests value
$res = $client->simple_request('system.status');
SKIP: {
    skip 'system.status response not HASH ref', 1 unless (ref($res) eq 'HASH');
    is($res->{total_requests}, 19,
       'system.status total_request correct at end of suite');
}

# Now that we're done, kill the server and exit
open(my $fh, "< $pid_file");
chomp(my $pid = <$fh>);
if ($pid =~ /^(\d+)$/)
{
    kill 'INT', $1;
}
else
{
    warn "WARNING: $pid_file appears corrupt, zombie processes may remain!\n";
}

exit;
