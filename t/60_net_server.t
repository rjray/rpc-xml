#!/usr/bin/env perl

# Test the RPC::XML::Server class with Net::Server rather than HTTP::Daemon

# This is run after the test suite for RPC::XML::Client, so we will use that
# for the client-side of the tests.

## no critic(RequireCheckedClose)
## no critic(RequireInterpolationOfMetachars)

use strict;
use warnings;
use subs qw(start_server find_port);

use Carp qw(carp croak);
use File::Spec;
use Module::Load;
use Test::More;

use RPC::XML::Server;
use RPC::XML::Client;

my ($dir, $srv, $pid_file, $log_file, $port, $client, $res, @keys, $meth, $list,
    $bucket, %seen, $srv_hostname);

if ($^O eq 'MSWin32')
{
    # Can't run this (reliably) under Windows:
    plan skip_all => 'Net::Server tests not reliable on Windows platform';
}
elsif (! eval { load Net::Server; 1; })
{
    # If they do not have Net::Server, quietly skip
    plan skip_all => 'Net::Server not available';
}
else
{
    # otherwise...
    plan tests => 30;
}

# Presently, there is a problem with Net::Server+IO::Socket::IP, when the IPv6
# entry for 'localhost' comes before the IPv4 entry in /etc/hosts. For now, to
# get through the tests, look for that combination and substitute 127.0.0.1 for
# 'localhost' (and hope they don't have a weird network configuration).
# See RT#105679.
if (eval { load IO::Socket::IP; 1; })
{
    carp 'Working around an issue with Net::Server+IO::Socket::IP';
    $srv_hostname = '127.0.0.1';
}
else
{
    $srv_hostname = 'localhost';
}

(undef, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
require File::Spec->catfile($dir, 'util.pl');

$pid_file  = File::Spec->catfile($dir, 'net_server.pid');
$log_file  = File::Spec->catfile($dir, 'net_server.log');
if (($port = find_port) == -1)
{
    croak 'No usable port found between 9000 and 11000, skipping';
}

unlink $log_file, $pid_file;

# All this, and we haven't even created a server object or run a test yet

$srv = RPC::XML::Server->new(no_http => 1);
# Technically, this is overkill. But if it fails everything else blows up:
isa_ok($srv, 'RPC::XML::Server');
if (! ref $srv)
{
    croak "Server allocation failed, cannot continue. Message was: $srv";
}

# All of these parameters are passed to the run() method of
# Net::Server::MultiType
start_server($srv,
             server_type => 'Single',
             log_file    => $log_file,
             log_level   => 4,
             pid_file    => $pid_file,
             port        => $port,
             host        => $srv_hostname,
             background  => 1);
sleep 1; # Allow time for server to spin up
# Unless we see "ok 2", we have a problem
ok(-e $pid_file, 'server started, PID file exists');
# After this point, we have the obligation of killing the server manually
$client = RPC::XML::Client->new("http://$srv_hostname:$port");
is($client->simple_request('system.identity'), $srv->product_tokens,
   'system.identity matches $srv->product_tokens');

# At this point, most of this is copied from the first server test suite
# (40_server.t). We do want to verify the full introspection API under
# Net::Server, though.

$res = $client->simple_request('system.listMethods');
@keys = $srv->list_methods;
is(ref($res), 'ARRAY', 'system.listMethods returned ARRAY ref');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not an ARRAY reference', 2;
    }

    is(scalar(@{$res}), scalar(@keys),
       'system.listMethods returned correct number of names');
    is(join(q{} => sort @{$res}), join(q{} => sort @keys),
       'system.listMethods returned matching set of names');
}

# Test the substring-parameter calling of system.listMethods
$res = $client->simple_request('system.listMethods', 'method');
is(ref($res), 'ARRAY', 'system.listMethods returned ARRAY ref');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not an ARRAY reference', 1;
    }

    is(join(q{,} => sort @{$res}), 'system.methodHelp,system.methodSignature',
       'system.listMethods with pattern returned correct set of names');
}

# Again, with a pattern that will produce no matches
$res = $client->simple_request('system.listMethods', 'none_will_match');
is(ref($res), 'ARRAY', 'system.listMethods returned ARRAY ref');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not an ARRAY reference', 1;
    }

    is(scalar(@{$res}), 0, 'system.listMethods with bad pattern returned none');
}

# system.status
$res = $client->simple_request('system.status');
@keys = qw(host port name version path date date_int started started_int
           total_requests methods_known);
is(ref($res), 'HASH', 'system.status returned HASH ref');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not a HASH reference', 2;
    }

    my @seen_keys = grep { defined $res->{$_} } @keys;
    ok(@keys == @seen_keys, 'system.status hash has correct keys');
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
    if (! ref $res)
    {
        skip 'server response not an ARRAY reference', 1;
    }

    is(join(q{} => @{$res}),
       $srv->get_method('system.identity')->{help} .
       $srv->get_method('system.status')->{help},
       'system.methodHelp with specific methods returns correctly');
}

# system.methodHelp with an invalid argument
$res = $client->send_request('system.methodHelp', 'system.bad');
isa_ok($res, 'RPC::XML::fault', 'system.methodHelp (bad arg) response');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not an RPC::XML data object', 1;
    }

    like($res->string(), qr/Method.*unknown/,
         'system.methodHelp (bad arg) has correct faultString');
}

# system.methodSignature
$res = $client->simple_request('system.methodSignature', 'system.methodHelp');
is(ref($res), 'ARRAY', 'system.methodSignature returned ARRAY ref');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not an ARRAY reference', 1;
    }

    my $return_sig = join q{} => sort map { join q{ } => @{$_} } @{$res};
    my $method_sig =
        join q{} => sort @{$srv->get_method('system.methodHelp')->{signature}};
    is($return_sig, $method_sig,
       'system.methodSignature return value correct');
}

# system.methodSignature, with an invalid request
$res = $client->send_request('system.methodSignature', 'system.bad');
isa_ok($res, 'RPC::XML::fault', 'system.methodSignature (bad arg) response');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not an RPC::XML data object', 1;
    }

    like($res->string(), qr/Method.*unknown/,
         'system.methodSignature (bad arg) has correct faultString');
}

# system.introspection
$list = $client->simple_request('system.introspection');
$bucket = 0;
%seen = ();
SKIP: {
    if (ref($list) ne 'ARRAY')
    {
        skip 'system.introspection call did not return ARRAY ref', 1;
    }

    for my $result (@{$list})
    {
        if ($seen{$result->{name}}++)
        {
            # If we somehow get the same name twice, that's a point off
            $bucket++;
            next;
        }

        $meth = $srv->get_method($result->{name});
        if ($meth)
        {
            my $result_sig = join q{} => sort @{$result->{signature}};
            my $method_sig = join q{} => sort @{$meth->{signature}};
            # A point off unless all three of these match
            if (($meth->{help} ne $result->{help}) ||
                    ($meth->{version} ne $result->{version}) ||
                    ($result_sig ne $method_sig))
            {
                $bucket++;
            }
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
is(ref($res), 'ARRAY', 'system.multicall returned ARRAY ref');
SKIP: {
    if (! ref $res)
    {
        skip 'server response not an ARRAY reference', 2;
    }

    is($res->[0], $srv->product_tokens,
       'system.multicall, first return value correct');
  SKIP: {
        if (ref($res->[1]) ne 'ARRAY')
        {
            skip 'system.multicall return value second index not ARRAY ref', 1;
        }

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
    if (ref($res) ne 'RPC::XML::fault')
    {
        skip 'system.multicall (recursion) response error, cannot test', 1;
    }

    like($res->string, qr/Recursive/,
         'system.multicall recursion attempt set correct faultString');
}

# system.multicall, with bad data on one of the call specifications
$res = $client->send_request('system.multicall',
                             [ { methodName => 'system.identity' },
                               { methodName => 'system.listMethods',
                                 params => 'intro' } ]);
SKIP: {
    if (ref($res) ne 'RPC::XML::fault')
    {
        skip 'system.multicall (bad data) response error, cannot test', 1;
    }

    like($res->string, qr/value for.*params.*not an array/i,
         'system.multicall bad param array set correct faultString');
}

# system.status, once more, to check the total_requests value
$res = $client->simple_request('system.status');
SKIP: {
    if (ref($res) ne 'HASH')
    {
        skip 'system.status response not HASH ref', 1;
    }

    is($res->{total_requests}, 19,
       'system.status total_request correct at end of suite');
}

# Now that we're done, kill the server and exit
if (open my $fh, '<', $pid_file)
{
    chomp(my $pid = <$fh>);
    close $fh;
    if ($pid =~ /^(\d+)$/)
    {
        kill 'INT', $1;
    }
    else
    {
        carp "WARNING: $pid_file appears corrupt, zombie processes may remain!";
    }
}
else
{
    carp "WARNING: Opening $pid_file failed: $! (zombie processes may remain)";
}

exit;
