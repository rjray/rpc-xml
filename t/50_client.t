#!/usr/bin/perl
# $Id$

# Test the RPC::XML::Client class

use strict;
use vars qw($dir $srv $child $port $cli $res $flag);
use subs qw(start_server find_port);

use Test;

BEGIN { plan tests => 19 }

use LWP;
require File::Spec;

require RPC::XML::Server;
require RPC::XML::Client;

(undef, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
require File::Spec->catfile($dir, 'util.pl');

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
ok(ref $cli);

# With no server yet at that port, test the failure modes
ok((! $cli->simple_request('system.identity')) && $RPC::XML::ERROR);
ok(! ref($cli->send_request('system.identity')));

# Test the error-handling callback
$cli->error_handler(sub { $res++ });
$res = 0;
$cli->simple_request('system.identity');
ok($res);

# Test clearing it
$cli->error_handler(undef);
$res = 0;
$cli->simple_request('system.identity');
ok(! $res);

# Test setting and clearing both with combined_handler
$cli->combined_handler(sub { 1 });
ok($cli->error_handler() && ($cli->error_handler() eq $cli->fault_handler()));

$cli->combined_handler(undef);
ok(! ($cli->error_handler() or $cli->fault_handler()));

# Cool so far. Create and spawn the server.
$srv = RPC::XML::Server->new(port => $port);
die "Failed to create server: $srv, stopped" unless (ref $srv);
$child = start_server($srv);

# NOW, this should work. Also, set $RPC::XML::ERROR to see if it clears
$RPC::XML::ERROR = 'foo';
ok($cli->simple_request('system.identity') eq $srv->product_tokens);
ok(! $RPC::XML::ERROR);

# Using send_request should yield an RPC::XML::string object with that value
$res = $cli->send_request('system.identity');
ok((ref($res) eq 'RPC::XML::string') &&
   ($res->value eq $srv->product_tokens));

# See what comes back from bad (but successful) calls
$res = $cli->simple_request('system.teaseMe');
ok((ref($res) eq 'HASH') &&
   (join(';', sort keys %$res) eq 'faultCode;faultString') &&
   ($res->{faultString} =~ /Unknown method/));

# As opposed to a fault object:
$res = $cli->send_request('system.teaseMe');
ok((ref($res) eq 'RPC::XML::fault') && ($res->string =~ /Unknown method/));

# Give the fault handler a whirl -- note the return value is the fault object
$cli->fault_handler(sub { $flag++ if ((ref($_[0]) eq 'RPC::XML::fault') &&
                                      ($_[0]->string =~ /Unknown method/));
                          $_[0] });
$flag = 0;
$res = $cli->send_request('system.pleaseMe');
# Did the callback run correctly?
ok($flag);
# Is the value returned correct?
ok((ref($res) eq 'RPC::XML::fault') && ($res->string =~ /Unknown method/));

# Last tests-- is the url() method working?
ok($cli->uri =~ m|http://localhost:$port/?|);
# does calling it as an accesor change itp at all?
ok($cli->uri =~ m|http://localhost:$port/?|);

$cli->uri('http://www.oreilly.com/RPC');
ok($cli->uri eq 'http://www.oreilly.com/RPC');

# Kill the server long enough to add a new method
kill 'INT', $child;
sleep 1; # Give system enough time to reclaim resources

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

if ($LWP::VERSION > 5.800)
{
    if (open($fh1, '<' . File::Spec->catfile($dir, 'svsm_text.gif')) and
	open($fh2, '<' . File::Spec->catfile($dir, 'svsm_text.gif')))
    {
	# Setting the size threshhold to the size of the GIF will guarantee a
	# file spool, since we're sending the GIF twice.
	$cli->message_file_thresh(-s $fh1);
	$cli->message_temp_dir($dir);

	$cli->uri("http://localhost:$port/");
	$res = $cli->send_request(cmpImg =>
				  RPC::XML::base64->new($fh1),
				  RPC::XML::base64->new($fh2));
	ok((ref($res) eq 'RPC::XML::boolean') && $res->value);

	# Force the compression threshhold down, to test that branch
	$cli->compress_requests(1);
	$cli->compress_thresh(-s $fh1);
	$res = $cli->send_request(cmpImg =>
				  RPC::XML::base64->new($fh1),
				  RPC::XML::base64->new($fh2));
	ok((ref($res) eq 'RPC::XML::boolean') && $res->value);
    }
    else
    {
	skip("Error opening svsm_text.gif: $!", 0);
	skip("Error opening svsm_text.gif: $!", 0);
    }
}
else
{
    skip("Message-to-file spooling broken with LWP < 5.801", 0);
    skip("Message-to-file spooling broken with LWP < 5.801", 0);
}

# Kill the server before exiting
kill 'INT', $child;

exit;
