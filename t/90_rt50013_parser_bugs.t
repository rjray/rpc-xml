#!/usr/bin/env perl

# https://rt.cpan.org/Ticket/Display.html?id=50013
#
# Ensure that RPC::XML::Parser::new() maintains backwards-compatibility

use strict;
use warnings;

use Carp qw(croak);
use Test::More;

use RPC::XML::Parser;

plan tests => 2;

my $parser;

# Since the changed behaviour was to die, to be safe use eval here
if (! eval { $parser = RPC::XML::Parser->new(); 1; })
{
    croak "Creating the parser died, cannot continue: $@";
}

isa_ok($parser, 'RPC::XML::Parser', 'Parser object');
isa_ok($parser, 'RPC::XML::Parser::XMLParser', 'Parser object');

exit;
