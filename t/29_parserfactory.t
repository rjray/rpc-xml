#!/usr/bin/perl

# Test the RPC::XML::ParserFactory class

use strict;
use vars qw($p $req $res $ret $ns $dir $vol $config %parsers);

use Test::More tests => 26;
use Symbol;
require File::Spec;
require IO::File;

use RPC::XML ':all';

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, '');

# Need read_config() from util.pl:
require File::Spec->catfile($dir, 'util.pl');

$config = read_config(File::Spec->catfile($dir, 'test.conf'));
# What parsers did we detect?
%parsers = map { $_ => 1 } @{$config->{parsers}};

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# RPC::XML::* classes, RPC::XML::Parser::XMLParser or any of the other
# parser-instance classes that are currently part of the distro.

# Start by testing with the XML::Parser wrapper, since that is the only one
# that is "required" (for now).
{
    use RPC::XML::ParserFactory;

    $p = RPC::XML::ParserFactory->new();
    isa_ok($p, 'RPC::XML::Parser',            '$p');
    isa_ok($p, 'RPC::XML::Parser::XMLParser', '$p');

    $req = RPC::XML::request->new('test.method');
    $ret = $p->parse($req->as_string);
    isa_ok($ret, 'RPC::XML::request', '$ret');
    is($ret->name, 'test.method', 'Correct request method name');

    $res = RPC::XML::response->new(RPC::XML::string->new('test response'));
    $ret = $p->parse($res->as_string);
    isa_ok($ret, 'RPC::XML::response', '$ret');
    is($ret->value->value, 'test response', 'Response value');

    # Test some badly-formed data
    my $tmp = $res->as_string;
    $tmp =~ s/methodResponse/mR/g;
    $ret = $p->parse($tmp);
    ok(!ref($ret), 'Bad XML did not parse');
    like($ret, qr/Unknown tag/, 'Parse failure returned error');
}

# For all the evals, to avoid namespace pollution, we'll keep incrementing
# this...
my $ns      = 'namespace0000';
my %aliases = (
    'XML::Parser' => [qw(XML::Parser xml::parser xmlparser)],
    'XML::LibXML' => [qw(XML::LibXML xml::libxml xmllibxml)],
    'XML::SAX'    => [qw(XML::SAX xml::sax xmlsax)],
);

# Test with the various aliases for XML::Parser
for my $alias (@{$aliases{'XML::Parser'}})
{
    $ns++;

    eval <<"EndOfEval1";
{
    package $ns;
    use RPC::XML::ParserFactory (class => $alias);

    \$main::p = RPC::XML::ParserFactory->new();
}
EndOfEval1

    isa_ok($p, 'RPC::XML::Parser',            "Alias $alias: \$p");
    isa_ok($p, 'RPC::XML::Parser::XMLParser', "Alias $alias: \$p");
}

# The non-xmlparser parsers are all optional, so skip their sets if the
# parser isn't in the config:
for my $parser (qw(XML::LibXML XML::SAX))
{
    SKIP:
    {
        skip "$parser not detected, tests skipped", 6
          unless $parsers{$parser};

        for my $alias (@{$aliases{$parser}})
        {
            $ns++;

            eval <<"EndOfEval1";
{
    package $ns;
    use RPC::XML::ParserFactory (class => $alias);

    \$main::p = RPC::XML::ParserFactory->new();
}
EndOfEval1

            isa_ok($p, 'RPC::XML::Parser',            "Alias $alias: \$p");
            isa_ok($p, 'RPC::XML::Parser::XMLParser', "Alias $alias: \$p");
        }
    }
}

exit 0;
