#!/usr/bin/perl

# https://rt.cpan.org/Ticket/Display.html?id=50013
#
# Ensure that RPC::XML::Parser::new() maintains backwards-compatibility

use strict;
use vars qw($parser);

use Test::More tests => 2;

use RPC::XML::Parser;

# Since the changed behaviour was to die, to be safe use eval here
eval { $parser = RPC::XML::Parser->new(); };

isa_ok($parser, 'RPC::XML::Parser', 'Parser object');
isa_ok($parser, 'RPC::XML::Parser::XMLParser', 'Parser object');

exit;
