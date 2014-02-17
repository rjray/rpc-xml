#!/usr/bin/env perl

use strict;
use warnings;

use Module::Load;
use Test::More;

# Verify that the individual modules will load

my @MODULES = qw(
    RPC::XML
    RPC::XML::Client
    RPC::XML::Parser
    RPC::XML::Parser::XMLParser
    RPC::XML::ParserFactory
    RPC::XML::Procedure
    RPC::XML::Server
);
my @APACHE_MODULES = qw(Apache::RPC::Server Apache::RPC::Status);
my @LIBXML_MODULES = qw(RPC::XML::Parser::XMLLibXML);

# If mod_perl is not available, Apache::RPC::Server cannot be blamed
my $do_apache = eval { load Apache; 1; };

# If XML::LibXML is not installed, also skip RPC::XML::Parser::XMLLibXML
my $do_libxml = eval { load XML::LibXML; 1; };

plan tests => (@MODULES + @APACHE_MODULES + @LIBXML_MODULES);

# Core modules
for my $module (@MODULES)
{
    use_ok $module;
}

# Test these only if XML::LibXML is available
SKIP: {
    if (! $do_libxml)
    {
        skip 'No XML::LibXML detected', scalar @LIBXML_MODULES;
    }

    for my $module (@LIBXML_MODULES)
    {
        use_ok $module;
    }
}

# Test these only if Apache (v1) is available
SKIP: {
    if (! $do_apache)
    {
        skip 'No mod_perl 1.X detected', scalar @APACHE_MODULES;
    }

    for my $module (@APACHE_MODULES)
    {
        use_ok $module;
    }
}

exit 0;
