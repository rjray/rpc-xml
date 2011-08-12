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

all_pod_files_ok();

exit;
