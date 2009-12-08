#!/usr/bin/perl

# Test that the syntax of our POD documentation is valid
use strict;
BEGIN {
	$|  = 1;
	$^W = 1;
}

use Test::More;
use Pod::Simple 3.07;
use Test::Pod 1.26;

# Don't run tests during end-user installs
plan skip_all => "Author tests not required for installation"
    unless ($ENV{AUTHOR_TESTING} or $ENV{RELEASE_TESTING});

all_pod_files_ok();

exit;
