#!/usr/bin/perl

use strict;
use vars qw(@MODULES @APACHE_MODULES $do_apache $do_libxml);

use Test::More;

# Verify that the individual modules will load

BEGIN
{
    @MODULES = qw(
        RPC::XML
        RPC::XML::Client
        RPC::XML::Parser
        RPC::XML::Parser::XMLLibXML
        RPC::XML::Parser::XMLParser
        RPC::XML::ParserFactory
        RPC::XML::Procedure
        RPC::XML::Server
    );
    @APACHE_MODULES = qw(Apache::RPC::Server Apache::RPC::Status);

    # If mod_perl is not available, Apache::RPC::Server cannot be blamed
    eval "use Apache";
    $do_apache = $@ ? 0 : 1;

    # If XML::LibXML is not installed, also skip RPC::XML::Parser::XMLLibXML
    eval "use XML::LibXML";
    $do_libxml = $@ ? 0 : 1;

    plan tests => (scalar(@MODULES) + scalar(@APACHE_MODULES));
}

# Core modules
for my $module (@MODULES)
{
  SKIP: {
        skip 'XML::LibXML not installed', 1
            if (($module eq 'RPC::XML::Parser::XMLLibXML') &&
                (! $do_libxml));

        use_ok($module);
    }
}

# Test these only if Apache (v1) is available
SKIP: {
    skip "No mod_perl 1.X detected", scalar(@APACHE_MODULES) unless $do_apache;

    use_ok($_) for (@APACHE_MODULES);
}

exit 0;
