#!/usr/bin/perl

# Test the RPC::XML::Client class

use strict;
use vars qw($dir $vol $srv $child $port $cli $res $flag);
use subs qw(start_server find_port);

use Test::More;

use LWP;
require File::Spec;

require RPC::XML::Server;
require RPC::XML::Client;

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, '');
require File::Spec->catfile($dir, 'util.pl');

plan tests => 27;

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# any other classes are done, only on the data and return values of this
# class under consideration, RPC::XML::Client. In this particular case, this
# means that we can safely use RPC::XML::Server in creating a suitable test
# environment.

# Start with some very basic things, before actually firing up a live server.
die "No usable port found between 9000 and 10000, skipping"
    if (($port = find_port) == -1);
$cli = RPC::XML::Client->new("http://localhost:$port");
$cli->timeout(5); #to prevent long waiting for non-existing server
isa_ok($cli, 'RPC::XML::Client', '$cli');

# With no server yet at that port, test the failure modes
ok((! $cli->simple_request('system.identity')) && $RPC::XML::ERROR,
   'Calling a server method without a server sets $RPC::XML::ERROR');
ok(! ref($cli->send_request('system.identity')),
   'send_request returns a non-ref value when there is no server');

# Test the error-handling callback
$cli->error_handler(sub { $res++ });
$res = 0;
$cli->simple_request('system.identity');
ok($res, 'error_handler callback system');

# Test clearing it
$cli->error_handler(undef);
$res = 0;
$cli->simple_request('system.identity');
ok(! $res, 'Clearing the error_handler callback system');

# Test setting and clearing both with combined_handler
$cli->combined_handler(sub { 1 });
ok($cli->error_handler() && ($cli->error_handler() eq $cli->fault_handler()),
   'combined_handler set both error_handler and fault_handler');

$cli->combined_handler(undef);
ok(! ($cli->error_handler() or $cli->fault_handler()),
   'combined_handler clears both error_handler and fault_handler');

# Check the getting/setting of the timeout() value on the underlying UA
is($cli->timeout(), $cli->useragent->timeout(),
   'Client timeout() method, fetching');
$cli->timeout(60);
is($cli->useragent->timeout(), 60, 'Client timeout() method, setting');

# Cool so far. Create and spawn the server.
$srv = RPC::XML::Server->new(host => 'localhost', port => $port);
die "Failed to create server: $srv, stopped" unless (ref $srv);
$child = start_server($srv);

# NOW, this should work. Also, set $RPC::XML::ERROR to see if it clears
$RPC::XML::ERROR = 'foo';
is($cli->simple_request('system.identity'), $srv->product_tokens,
   'simple_request/system.identity returns correct value');
ok(! $RPC::XML::ERROR,
   'simple_request/system.identity left $RPC::XML::ERROR empty');

# Using send_request should yield an RPC::XML::string object with that value
$res = $cli->send_request('system.identity');
isa_ok($res, 'RPC::XML::string', 'system.identity response');
SKIP: {
    skip 'Client response not a RPC::XML data object', 1
        unless ref $res;
    is($res->value, $srv->product_tokens,
       'system.identity response is correct');
}

unless (ref $res)
{
    # Assume that if an error occurred, the server might be in a confused
    # state. Kill and restart it.
    stop_server($child);
    $child = start_server($srv);
}

# See what comes back from bad (but successful) calls
$res = $cli->simple_request('system.bad');
isa_ok($res, 'HASH', 'simple_request/system.bad response');
SKIP: {
    skip 'Client response was not a RPC::XML data object', 2
        unless ref $res;
    is(join(';', sort keys %$res), 'faultCode;faultString',
       'simple_request/system.bad hashref has correct keys');
    like($res->{faultString}, qr/Unknown method/,
         'simple_request/system.bad set correct faultString');
}

unless (ref $res)
{
    # Assume that if an error occurred, the server might be in a confused
    # state. Kill and restart it.
    stop_server($child);
    $child = start_server($srv);
}

# As opposed to a fault object:
$res = $cli->send_request('system.bad');
isa_ok($res, 'RPC::XML::fault', 'send_request/system.bad response');
SKIP: {
    skip 'Client response not a RPC::XML data object', 1
        unless ref $res;
    like($res->string, qr/Unknown method/,
         'send_request/system.bad set correct string() property');
}

unless (ref $res)
{
    # Assume that if an error occurred, the server might be in a confused
    # state. Kill and restart it.
    stop_server($child);
    $child = start_server($srv);
}

# Give the fault handler a whirl -- note the return value is the fault object
$cli->fault_handler(sub { $flag++ if ((ref($_[0]) eq 'RPC::XML::fault') &&
                                      ($_[0]->string =~ /Unknown method/));
                          $_[0] });
$flag = 0;
$res = $cli->send_request('system.bad');
# Did the callback run correctly?
ok($flag, 'fault_handler correctly set $flag');
# Is the value returned correct?
isa_ok($res, 'RPC::XML::fault', 'fault_handler returned value');
SKIP: {
    skip 'Client response not a RPC::XML data object', 1
        unless ref $res;
    like($res->string, qr/Unknown method/,
         'fault_handler object has correct faultString');
}

unless (ref $res)
{
    # Assume that if an error occurred, the server might be in a confused
    # state. Kill and restart it.
    stop_server($child);
    $child = start_server($srv);
}

# Last tests-- is the url() method working?
like($cli->uri, qr|http://localhost(\.localdomain)?:$port/?|,
     'RPC::XML::Client::uri method return value is correct');

# does calling it as an accesor change it at all?
$cli->uri('http://www.oreilly.com/RPC');
is($cli->uri, 'http://www.oreilly.com/RPC',
   'RPC::XML::Client::uri changes as expected');

# Kill the server long enough to add a new method
stop_server($child);

use Digest::MD5;

$srv->add_method({ name => 'cmpImg',
                   signature => [ 'boolean base64 base64' ],
                   code => sub {
                       my ($self, $img1, $img2) = @_;

                       return (Digest::MD5::md5_hex($img1) eq
                               Digest::MD5::md5_hex($img2));
                   } });
$child = start_server($srv);

use Symbol;
my ($fh1, $fh2) = (gensym, gensym);

SKIP: {
    skip 'Message-to-file spooling broken with LWP < 5.801', 4
        unless ($LWP::VERSION > 5.800);

    open($fh1, '<' . File::Spec->catfile($dir, 'svsm_text.gif'));
    open($fh2, '<' . File::Spec->catfile($dir, 'svsm_text.gif'));
  SKIP: {
        skip "Error opening svsm_text.gif: $!", 4
            unless ($fh1 and $fh2);
        # Setting the size threshhold to the size of the GIF will guarantee a
        # file spool, since we're sending the GIF twice.
        $cli->message_file_thresh(-s $fh1);
        $cli->message_temp_dir($dir);

        $cli->uri("http://localhost:$port/");
        $res = $cli->send_request(cmpImg =>
                                  RPC::XML::base64->new($fh1),
                                  RPC::XML::base64->new($fh2));
        isa_ok($res, 'RPC::XML::boolean', 'cmpImg return value');
      SKIP: {
            skip 'Client response not a RPC::XML data object', 1
                unless ref($res);
            ok($res->value, 'cmpImg, file spooling, correct return');
        }

        # Force the compression threshhold down, to test that branch
        $cli->compress_requests(1);
        $cli->compress_thresh(-s $fh1);
        $res = $cli->send_request(cmpImg =>
                                  RPC::XML::base64->new($fh1),
                                  RPC::XML::base64->new($fh2));
        isa_ok($res, 'RPC::XML::boolean', 'cmpImg return value');
      SKIP: {
            skip 'Client response not a RPC::XML data object', 1
                unless ref($res);
            ok($res->value, 'cmpImg, file spooling, correct return');
        }
    }
}

# Kill the server before exiting
stop_server($child);

exit;
