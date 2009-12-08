#!/usr/bin/perl

# Test that the module passes perlcritic
use strict;
BEGIN {
	$|  = 1;
	$^W = 1;
}

use Perl::Critic 1.098;
use Test::Perl::Critic 1.01;
use Test::More;

# Don't run tests during end-user installs
plan skip_all => "Author tests not required for installation"
    unless ($ENV{AUTHOR_TESTING} or $ENV{RELEASE_TESTING});


all_critic_ok();

exit;
