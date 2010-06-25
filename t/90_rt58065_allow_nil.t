#!/usr/bin/perl

# https://rt.cpan.org/Ticket/Display.html?id=58065
#
# Test that the parser-factory instance classes allow the parsing of the
# <nil /> tag whether $RPC::XML::ALLOW_NIL is set or not. This is to allow
# liberal acceptance of the tag in what we take in. Production of the tag is
# still limited to only when the flag is set.

use strict;
use vars qw($parser $req_message $res_message $parsed);

use Test::More tests => 8;

# Use classes from here directly to create test messages for parsing
use RPC::XML;

# This factory-class-instance should always be present
require RPC::XML::Parser::XMLParser;
# This one may not be present
my $can_libxml = eval {
    require RPC::XML::Parser::XMLLibXML;
    1;
};

# Create mock request and response messages that contain nils in them by first
# setting the flag. We'll then unset the flag for the tests.

$RPC::XML::ALLOW_NIL = 1;

$req_message = RPC::XML::request->new(
    'foo',
    RPC::XML::nil->new()
);
$res_message = RPC::XML::response->new(
    RPC::XML::nil->new()
);

$RPC::XML::ALLOW_NIL = 0;

# To test this, instantiate each parser then call the ->parse() method with
# both the request and response message that contain nil tags.

# First test the class we always have, RPC::XML::Parser::XMLParser
$parser = RPC::XML::Parser::XMLParser->new();

# Test-parse the request message
$parsed = $parser->parse($req_message->as_string);

isa_ok($parsed, 'RPC::XML::request');
SKIP: {
    skip 'Parsed value corrupted, cannot test nil value', 1
        unless (ref($parsed) eq 'RPC::XML::request');
    isa_ok($parsed->args->[0], 'RPC::XML::nil');
}

# Test-parse the response message
$parsed = $parser->parse($res_message->as_string);

isa_ok($parsed, 'RPC::XML::response');
SKIP: {
    skip 'Parsed value corrupted, cannot test nil value', 1
        unless (ref($parsed) eq 'RPC::XML::response');
    isa_ok($parsed->value, 'RPC::XML::nil');
}

# Next, test RPC::XML::Parser::XMLLibXML (which we might not have)
SKIP: {
    skip 'XML::LibXML not installed', 4
        unless $can_libxml;

    $parser = RPC::XML::Parser::XMLLibXML->new();

    # Test-parse the request message
    $parsed = $parser->parse($req_message->as_string);

    isa_ok($parsed, 'RPC::XML::request');
  SKIP: {
        skip 'Parsed value corrupted, cannot test nil value', 1
            unless (ref($parsed) eq 'RPC::XML::request');
        isa_ok($parsed->args->[0], 'RPC::XML::nil');
    }

    # Test-parse the response message
    $parsed = $parser->parse($res_message->as_string);

    isa_ok($parsed, 'RPC::XML::response');
  SKIP: {
        skip 'Parsed value corrupted, cannot test nil value', 1
            unless (ref($parsed) eq 'RPC::XML::response');
        isa_ok($parsed->value, 'RPC::XML::nil');
    }
}

exit;
