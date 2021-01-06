#!/usr/bin/perl
###############################################################################
#
# This is the MakeMaker skeleton for the RPC-XML extension. Besides the usual
# tricks, this has to add rules to make the *.xpl files from *.code in the
# methods/ subdir, as well as get them into a place where they get installed
# correctly.
#
###############################################################################

use 5.008008;
use strict;
use warnings;

use ExtUtils::MakeMaker;
use File::Spec;
use File::Find;

our $VERSION = '0.82';

my ($vol, $dir, undef) = File::Spec->splitpath(File::Spec->rel2abs($0));
$dir                   = File::Spec->catpath($vol, $dir, q{});

my $libxml_avail = eval {
    require XML::LibXML;
    1;
};
if (! $libxml_avail) {
    print {*STDERR} <<"END";
@@@@@
        XML::LibXML not found

        You may ignore the warnings about XML::LibXML not being present, if
        you plan only to use the XML::Parser-based parsing engine. The use
        of XML::LibXML is optional.
@@@@@

END
}

my $CLEAN = 'pod2html-* *.html *.spec *.rpm rpmrc rpmmacro *.log t/*.log ' .
    't/*.pid META.yml META.json MYMETA.yml MYMETA.json *.ppd cover_db ';

my @scripts = (File::Spec->catfile(qw(etc make_method)));
$CLEAN  .= File::Spec->catfile(qw(methods *.xpl));

my @PM_FILES = ();

find(
    sub {
        if (-f and /[.]pm$/) { push @PM_FILES, $File::Find::name }
    },
    'lib'
);

# Exclude Apache2 stuff until it's ready for deployment
@PM_FILES = grep { ! /Apache2/ } @PM_FILES;

my %PM_FILES = ();
for my $file (@PM_FILES) {
    (my $temp = $file) =~ s/^lib/\$\(INST_LIB\)/;
    $PM_FILES{$file} = $temp;
}

# Handle the method code in "methods" specially:
find(
    sub {
        if (-f and /[.]base$/) {
            s/[.]base$//;
            $PM_FILES{File::Spec->catfile('methods', "$_.xpl")} =
                File::Spec->catfile(qw($(INST_LIB) RPC XML), "$_.xpl");
        }
    },
    'methods'
);
# Anything stuck under "lib" is more generic
find(
    sub {
        if (-f and /[.]base$/) {
            (my $name = $File::Find::name) =~ s/base$/xpl/;
            (my $tmp = $name) =~ s/^lib/\$(INST_LIB)/;
            $PM_FILES{$name} = $tmp;
            $CLEAN .= " $name";
        }
    },
    'lib'
);

WriteMakefile(
    NAME      => 'RPC::XML',
    VERSION   => $VERSION,
    AUTHOR    => 'Randy J. Ray',
    ABSTRACT  => 'Data, client and server classes for XML-RPC',
    EXE_FILES => \@scripts,
    PM        => \%PM_FILES,
    PREREQ_PM => {
        'Carp'          => 0,
        'Scalar::Util'  => 1.55,
        'HTTP::Daemon'  => 6.12,
        'HTTP::Message' => 6.26,
        'LWP'           => 6.51,
        'Socket'        => 0,
        'XML::Parser'   => 2.46,
        'Module::Load'  => 0.36,
    },
    CONFIGURE_REQUIRES => {
        'ExtUtils::MakeMaker' => 7.56,
    },
    TEST_REQUIRES => {
        'ExtUtils::MakeMaker' => 7.56,
        'IO::Socket::IP'      => 0,
        'Test::More'          => 1.302183,
    },
    dist      => { COMPRESS => 'gzip -9f' },
    clean     => { FILES => $CLEAN },
    LICENSE   => 'perl',
    MIN_PERL_VERSION => 5.008008,
    META_MERGE => {
        recommends => {
            'XML::LibXML'               => '2.0206',
            'DateTime'                  => '1.54',
            'DateTime::Format::ISO8601' => '0.15',
        },
        resources => {
            homepage   => 'http://github.com/rjray/rpc-xml',
            bugtracker => 'https://github.com/rjray/rpc-xml/issues',
            repository => 'http://github.com/rjray/rpc-xml',
        }
    },
);

sub MY::post_initialize
{
    my $self = shift;

    my @text;
    my $makemeth = File::Spec->catfile(qw(etc make_method));

    push @text,
        '.SUFFIXES: .xpl .base',
        q{},
        '.base.xpl:',
        "\t\$(PERL) $makemeth --base=\$*",
        q{};

    return join "\n", @text;
}

sub MY::postamble
{
    my $self = shift;

    my @text;
    my $makemeth = File::Spec->catfile(qw(etc make_method));

    # Create the dependancy rules for the methods/XPL files
    for (sort grep { /[.]xpl$/ } keys %PM_FILES) {
        s/[.]xpl$//;
        push @text, "$_.xpl: $_.base $_.help $_.code $makemeth";
    }

    return join "\n", @text;
}
