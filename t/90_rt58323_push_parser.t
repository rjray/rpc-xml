#!/usr/bin/env perl

# https://rt.cpan.org/Ticket/Display.html?id=58323
#
# Test that the parser-factory instance classes correctly cause errors when
# passed null strings or 0 as an argument to parse().

use strict;
use warnings;

use Module::Load;
use Test::More;

use RPC::XML::Parser::XMLParser;

plan tests => 4;

my ($parser, $parse_result);
my $can_libxml = eval { load RPC::XML::Parser::XMLLibXML; 1; };

# To test this, instantiate each parser then call the ->parse() method with
# both a null string and with 0 as an argument. Each call should throw an
# error about failed parsing. If they don't, the test has failed.

# First test the class we always have, RPC::XML::Parser::XMLParser
$parser = RPC::XML::Parser::XMLParser->new();

# Empty string
$parse_result = $parser->parse(q{});
ok(! ref($parse_result), 'RPC::XML::Parser::XMLParser null string');

# Zero
$parse_result = $parser->parse(0);
ok(! ref($parse_result), 'RPC::XML::Parser::XMLParser zero value');

# Next, test RPC::XML::Parser::XMLLibXML (which we might not have)
SKIP: {
    if (! $can_libxml)
    {
        skip 'XML::LibXML not installed', 2;
    }

    $parser = RPC::XML::Parser::XMLLibXML->new();

    # Empty string
    $parse_result = $parser->parse(q{});
    ok(! ref($parse_result), 'RPC::XML::Parser::XMLLibXML null string');

    # Zero
    $parse_result = $parser->parse(0);
    ok(! ref($parse_result), 'RPC::XML::Parser::XMLLibXML zero value');
}

exit;
