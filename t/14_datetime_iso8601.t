#!/usr/bin/env perl

# Test the date-parsing facilities provided by the DateTime::Format::ISO8601
# module, if available

use strict;
use warnings;

use Module::Load;
use Test::More;

use RPC::XML;

my ($obj, @values, $formatter);

my $datetime_format_iso8601_avail = eval { load DateTime::Format::ISO8601; 1; };
# Do not run this suite if the package is not available
if (! $datetime_format_iso8601_avail)
{
    plan skip_all => 'DateTime::Format::ISO8601 not available';
}

# Otherwise, we have to calculate our tests from the content after __DATA__:
while (defined(my $line = <DATA>))
{
    next if ($line =~ /^#/);
    chomp $line;
    next if (! $line);
    push @values, [ split /[|]/, $line ];
}

plan tests => (scalar(@values) * 2);

# Create a formatter from the DateTime::Format::ISO8601 package, we'll use it
# to determine what the constructor *should* return:
$formatter = DateTime::Format::ISO8601->new();

for my $test (0 .. $#values)
{
    my ($input, $is_error) = @{$values[$test]};

    $obj = RPC::XML::datetime_iso8601->new($input);
    if (! $is_error)
    {
        my $match = $formatter->parse_datetime($input);
        $match =~ s/-//g;

        isa_ok($obj, 'RPC::XML::datetime_iso8601', "Input $test \$obj");
        is($obj->value, $match, "Input '$input' yielded correct value");
    }
    else
    {
        ok(! ref($obj), "Input $test yielded no object");
        like($RPC::XML::ERROR, qr/Malformed data [(]$input[)]/,
             "Input '$input' yielded correct error message");
    }
}

exit 0;

__DATA__
# Format is:
# <Input value>|<Error if set>
#
# If the second field is non-blank, then the input should yield an error
#
# I am skipping some of the sillier formats, as I don't care if people use them
# and get unexpected results. Caveat Programmer, and all that...
20110820
2011-08-20
2011-08
2011
110820
11-08-20
-1108
-11-08
--0820
--08-20
--08
---20
2011232
2011-232
11232
11-232
-232
2011W336
2011-W33-6
2011W33
2011-W33
11W336
11-W33-6
11W33
11-W33
-1W336
-1-W33-6
-1W33
-1-W33
-W336
-W33-6
17:55:55
17:55
175555,50
17:55:55,50
175555.50
1755.50
17:55.50
17.50
-55:00
-5500,50
-55.50
--00.0
175555Z
17:55:55Z
1755Z
17:55Z
17Z
175555.0Z
17:55:55.0Z
175555-0700
17:55:55-07:00
175555-07
17:55:55-07
175555.0-0700
17:55:55.0-07:00
17,01|bad
20110820175555|bad
