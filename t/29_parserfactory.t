#!/usr/bin/env perl

# Test the RPC::XML::ParserFactory class

## no critic(RequireInterpolationOfMetachars)
## no critic(ProhibitStringyEval)
## no critic(RequireCheckingReturnValueOfEval)

use strict;
use warnings;

use Module::Load;
use Test::More;
use File::Spec;

use RPC::XML ':all';
use RPC::XML::ParserFactory;

plan tests => 38;

my ($req, $res, $ret, $ns, $dir, $vol, %aliases, %parsers);
# This one will be referenced from outside of main::, so it has to be visible:
our $p; ## no critic(ProhibitPackageVars)

($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir = File::Spec->catpath($vol, $dir, q{});
unshift @INC, $dir;

%parsers = (
    'XML::Parser' => 1,
);

# See if we should run tests dependent on XML::LibXML
if (eval { load XML::LibXML; 1; })
{
    $parsers{'XML::LibXML'} = 1;
}

# The organization of the test suites is such that we assume anything that
# runs before the current suite is 100%. Thus, no consistency checks on
# RPC::XML::* classes, RPC::XML::Parser::XMLParser or any of the other
# parser-instance classes that are currently part of the distro.

# First let's squeeze in a negative test, to see what happens when an attempt
# to load a valid parser fails
unshift @INC, sub {
    die "Force-failing RPC::XML::Parser::XMLParser\n"
        if ($_[1] eq 'RPC/XML/Parser/XMLParser.pm');
    return;
};
$p = RPC::XML::ParserFactory->new(class => 'XML::Parser');
ok(! $p, 'Factory correctly failed when it could not load parser class');
like($RPC::XML::ERROR, qr/loading RPC::XML::Parser::XMLParser/,
     'Correct error message');
# Now clear out that pesky closure so the rest of the tests succeed
shift @INC;

# Now start by testing with the XML::Parser wrapper, since that is the only one
# that is "required" (for now).
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

# For all the evals, to avoid namespace pollution, we'll keep incrementing
# this...
$ns      = 'namespace0000';
%aliases = (
    'XML::Parser' => [ qw(XML::Parser xml::parser xmlparser) ],
    'XML::LibXML' => [ qw(XML::LibXML xml::libxml xmllibxml) ],
);

# Test with the various aliases for XML::Parser
for my $alias (@{$aliases{'XML::Parser'}})
{
    $ns++;
    undef $p;

    eval <<"END_OF_EVAL";
{
    package $ns;
    use RPC::XML::ParserFactory (class => '$alias');

    \$main::p = RPC::XML::ParserFactory->new();
}
END_OF_EVAL

    isa_ok($p, 'RPC::XML::Parser',            "Alias $alias: \$p");
    isa_ok($p, 'RPC::XML::Parser::XMLParser', "Alias $alias: \$p");
}

# The non-xmlparser parsers are all optional, so skip their sets if the
# parser isn't in the config:
for my $parser (qw(XML::LibXML))
{
    (my $factory_class = $parser) =~ s/:://g;
    $factory_class = "RPC::XML::Parser::$factory_class";
  SKIP:
    {
        if (! $parsers{$parser})
        {
            skip "$parser not detected, tests skipped", 6;
        }

        for my $alias (@{$aliases{$parser}})
        {
            $ns++;
            undef $p;

            eval <<"END_OF_EVAL";
{
    package $ns;
    use RPC::XML::ParserFactory qw($alias);

    \$main::p = RPC::XML::ParserFactory->new();
}
END_OF_EVAL

            isa_ok($p, 'RPC::XML::Parser', "Alias $alias: \$p");
            isa_ok($p, $factory_class,     "Alias $alias: \$p");
        }
    }
}

# This block makes sure that we can new() a parser with a specific alias
for my $parser (qw(XML::Parser XML::LibXML))
{
    (my $factory_class = $parser) =~ s/:://g;
    $factory_class = "RPC::XML::Parser::$factory_class";
  SKIP:
    {
        if (! $parsers{$parser})
        {
            skip "$parser not detected, tests skipped", 6;
        }

        for my $alias (@{$aliases{$parser}})
        {
            $p = RPC::XML::ParserFactory->new(class => $alias);

            isa_ok($p, 'RPC::XML::Parser', "New'ing $alias: \$p");
            isa_ok($p, $factory_class,     "New'ing $alias: \$p");
        }
    }
}

# Some negative tests
$p = RPC::XML::ParserFactory->new(class => 'DoesNotExist');
ok(! $p, 'Factory-new fails with bad class argument');
like($RPC::XML::ERROR, qr/Error loading DoesNotExist/,
     'Correct error message');
$p = RPC::XML::ParserFactory->new(class => 'BadParserClass');
ok(! $p, 'Factory-new fails with a bad parser class');
like($RPC::XML::ERROR, qr/is not a sub-class of/, 'Correct error message');

exit 0;
