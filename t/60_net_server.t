#!/usr/bin/perl

# Test the RPC::XML::Server class with Net::Server rather than HTTP::Daemon

# This is run after the test suite for RPC::XML::Client, so we will use that
# for the client-side of the tests.

use strict;
use vars qw($dir $srv $pid_file $log_file $port $client $res @keys $meth $list
            $bucket %seen);
use subs qw(start_server find_port);

use File::Spec;
use Test;

BEGIN
{
    eval "use Net::Server";
    if ($@)
    {
        # If they do not have Net::Server, quietly skip
        plan tests => 0;
        exit;
    }
    else
    {
        plan tests => 17;
    }
}

require RPC::XML::Server;
require RPC::XML::Client;

(undef, $dir, undef) = File::Spec->splitpath($0);
require File::Spec->catfile($dir, 'util.pl');

$pid_file  = File::Spec->catfile($dir, 'net_server.pid');
$log_file  = File::Spec->catfile($dir, 'net_server.log');
die "No usable port found between 9000 and 10000, skipping"
    if (($port = find_port) == -1);

unlink $log_file if (-e $log_file);
unlink $pid_file if (-e $pid_file);

# All this, and we haven't even created a server object or run a test yet

$srv = RPC::XML::Server->new(no_http => 1);
# Technically, this is overkill. But it never hurts...
ok(ref $srv);

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
ok(-e $pid_file);
# After this point, we have the obligation of killing the server manually
$client = RPC::XML::Client->new("http://localhost:$port");
ok($client->simple_request('system.identity') eq $srv->product_tokens);

# At this point, most of this is copied from the first server test suite (40).
# We do want to verify the full introspection API under Net::Server, though.

$res = $client->simple_request('system.listMethods');
@keys = $srv->list_methods;
ok((ref($res) eq 'ARRAY') && (@$res == @keys) &&
   (join('', sort @$res) eq join('', sort @keys)));

# Test the substring-parameter calling of system.listMethods
$res = $client->simple_request('system.listMethods', 'method');
ok((ref($res) eq 'ARRAY') &&
   (join(',', sort @$res) eq 'system.methodHelp,system.methodSignature'));

# Again, with a pattern that will produce no matches
$res = $client->simple_request('system.listMethods', 'microsquirt');
ok((ref($res) eq 'ARRAY') && (@$res == 0));

# system.status
$res = $client->simple_request('system.status');
@keys = qw(host port name version path date date_int started started_int
           total_requests methods_known);
ok((ref($res) eq 'HASH') && (grep(defined $res->{$_}, @keys) == @keys) &&
   ($res->{total_requests} == 5));

# system.methodHelp
$res = $client->simple_request('system.methodHelp', 'system.identity');
ok($res eq $srv->get_method('system.identity')->{help});

# system.methodHelp with multiple arguments
$res = $client->simple_request('system.methodHelp',
                             [ 'system.identity', 'system.status' ]);
ok(join('', @$res) eq
   $srv->get_method('system.identity')->{help} .
   $srv->get_method('system.status')->{help});

# system.methodHelp with an invalid argument
$res = $client->send_request('system.methodHelp', 'system.teaseMe');
ok(ref($res) && $res->is_fault() && $res->string() =~ /Method.*unknown/);

# system.methodSignature
$res = $client->simple_request('system.methodSignature', 'system.methodHelp')
    ;
ok(join('', sort @$res) eq
   join('', sort @{ $srv->get_method('system.methodHelp')->{signature} }));

# system.methodSignature, with an invalid request
$res = $client->send_request('system.methodSignature', 'system.pleaseMe');
ok(ref($res) && $res->is_fault() && $res->string() =~ /Method.*unknown/);

# system.introspection
$list = $client->simple_request('system.introspection');
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

# system.multicall
$res = $client->simple_request('system.multicall',
                               [ { methodName => 'system.identity' },
                                 { methodName => 'system.listMethods',
                                   params => [ 'intro' ] } ]);
ok((ref($res) eq 'ARRAY') && ($res->[0] eq $srv->product_tokens) &&
   ($res->[1]->[0] eq 'system.introspection'));

# system.multicall, with an attempt at illegal recursion
$res = $client->send_request('system.multicall',
                             [ { methodName => 'system.identity' },
                               { methodName => 'system.multicall',
                                 params => [ 'intro' ] } ]);
ok($res->is_fault && $res->string =~ /Recursive/);

# system.multicall, with bad data on one of the call specifications
$res = $client->send_request('system.multicall',
                             [ { methodName => 'system.identity' },
                               { methodName => 'system.listMethods',
                                 params => 'intro' } ]);
ok($res->is_fault && $res->string =~ /value for.*params.*not an array/i);

# system.status, once more, to check the total_requests value
$res = $client->simple_request('system.status');
ok($res->{total_requests} == 19);

# Now that we're done, kill the server and exit
kill 'INT', `cat $pid_file`;
exit;
