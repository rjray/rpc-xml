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

all_critic_ok();

exit;
