#!/usr/bin/perl

# Test that our META.yml file matches the specification
use strict;
BEGIN {
	$|  = 1;
	$^W = 1;
}

use Test::CPAN::Meta 0.12;
use Test::More;

plan skip_all => "No META.yml file found" unless (-f 'META.yml');

meta_yaml_ok();

exit;
