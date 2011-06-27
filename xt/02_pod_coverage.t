#!/usr/bin/perl

use Test::More;
use Test::Pod::Coverage;

plan skip_all => "Author tests not required for installation"
    unless ($ENV{AUTHOR_TESTING} or $ENV{RELEASE_TESTING});

plan tests => 12;

pod_coverage_ok('Apache::RPC::Server' => { also_private => [ 'debug' ] } =>
                'Apache::RPC::Server');
pod_coverage_ok('Apache::RPC::Status' => 'Apache::RPC::Status');
pod_coverage_ok('RPC::XML' =>
                { also_private => [ qr/^RPC_/ ] },
                'RPC::XML');
pod_coverage_ok('RPC::XML::Client' => { also_private => [ qr/^compress/ ] } =>
                'RPC::XML::Client');
pod_coverage_ok('RPC::XML::Function' =>
                { also_private => [ qw(make_sig_table) ] } =>
                'RPC::XML::Function');
pod_coverage_ok('RPC::XML::Method' => 'RPC::XML::Method');
pod_coverage_ok('RPC::XML::ParserFactory' => 'RPC::XML::ParserFactory');
pod_coverage_ok('RPC::XML::Parser' => 'RPC::XML::Parser');
pod_coverage_ok('RPC::XML::Parser::XMLParser' =>
                { also_private =>
                  [ qr/^(tag|message)_/,
                    qw(char_data error extern_ent final stack_error) ] } =>
                'RPC::XML::Parser::XMLParser');
pod_coverage_ok('RPC::XML::Parser::XMLLibXML' =>
                { also_private => [ qr/^dom_/ ] } =>
                'RPC::XML::Parser::XMLLibXML');
pod_coverage_ok('RPC::XML::Procedure' =>
                { also_private => [ qw(load_XPL_file make_sig_table) ] } =>
                'RPC::XML::Procedure');
pod_coverage_ok('RPC::XML::Server' => { also_private => [ 'compress_re' ] } =>
                'RPC::XML::Server');

exit;
