#!/usr/bin/env perl

# https://rt.cpan.org/Ticket/Display.html?id=58065
#
# Test that the parser-factory instance classes allow the parsing of the
# <nil /> tag whether $RPC::XML::ALLOW_NIL is set or not. This is to allow
# liberal acceptance of the tag in what we take in. Production of the tag is
# still limited to only when the flag is set.

## no critic(RequireInterpolationOfMetachars)

use strict;
use warnings;

use Module::Load;
use Test::More;

use RPC::XML;
use RPC::XML::Parser::XMLParser;

plan tests => 8;

my ($parser, $req_message, $res_message, $parsed);
my $can_libxml = eval { load RPC::XML::Parser::XMLLibXML; 1; };

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

isa_ok($parsed, 'RPC::XML::request', '$parsed content');
SKIP: {
    if (ref($parsed) ne 'RPC::XML::request')
    {
        skip 'Parsed value corrupted, cannot test nil value', 1;
    }

    isa_ok($parsed->args->[0], 'RPC::XML::nil', '$parsed->args->[0]');
}

# Test-parse the response message
$parsed = $parser->parse($res_message->as_string);

isa_ok($parsed, 'RPC::XML::response', '$parsed content');
SKIP: {
    if (ref($parsed) ne 'RPC::XML::response')
    {
        skip 'Parsed value corrupted, cannot test nil value', 1;
    }

    isa_ok($parsed->value, 'RPC::XML::nil', '$parsed->value');
}

# Next, test RPC::XML::Parser::XMLLibXML (which we might not have)
SKIP: {
    if (! $can_libxml)
    {
        skip 'XML::LibXML not installed', 4;
    }

    $parser = RPC::XML::Parser::XMLLibXML->new();

    # Test-parse the request message
    $parsed = $parser->parse($req_message->as_string);

    isa_ok($parsed, 'RPC::XML::request', '$parsed content');
  SKIP: {
        if (ref($parsed) ne 'RPC::XML::request')
        {
            skip 'Parsed value corrupted, cannot test nil value', 1;
        }

        isa_ok($parsed->args->[0], 'RPC::XML::nil', '$parsed->args->[0]');
    }

    # Test-parse the response message
    $parsed = $parser->parse($res_message->as_string);

    isa_ok($parsed, 'RPC::XML::response', '$parsed content');
  SKIP: {
        if (ref($parsed) ne 'RPC::XML::response')
        {
            skip 'Parsed value corrupted, cannot test nil value', 1;
        }

        isa_ok($parsed->value, 'RPC::XML::nil', '$parsed->value');
    }
}

exit;
