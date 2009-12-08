#!/usr/bin/perl

# Test that our declared minimum Perl version matches our syntax
use strict;
BEGIN {
	$|  = 1;
	$^W = 1;
}

use Perl::MinimumVersion 1.20;
use Test::MinimumVersion 0.008;
use Test::More;

# Don't run tests during end-user installs
plan skip_all => "Author tests not required for installation"
    unless ($ENV{AUTHOR_TESTING} or $ENV{RELEASE_TESTING});

all_minimum_version_from_metayml_ok();

exit;
