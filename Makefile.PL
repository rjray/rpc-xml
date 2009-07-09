###############################################################################
#
# This is the MakeMaker skeleton for the RPC-XML extension. Besides the usual
# tricks, this has to add rules to make the *.xpl files from *.code in the
# methods/ subdir, as well as get them into a place where they get installed
# correctly.
#
###############################################################################
use ExtUtils::MakeMaker;
use File::Spec;
use File::Find;
use Cwd 'cwd';

eval "require LWP;";
if ($LWP::VERSION and $LWP::VERSION < 5.801)
{
    print STDERR <<END;
@@@@@
        Please note!

        Due to a bug fixed in version 5.801 of LWP, using the spool-to-file
        feature of RPC::XML::Client is broken. The relevant tests will be
        skipped in the test suite, but unless you upgrade your LWP you
        should not attempt to use this feature.
@@@@@

END
}

eval "use XML::LibXML;";
if ($@ or $XML::LibXML::VERSION < 1.58)
{
    print STDERR <<END;
@@@@@
        Please note!

        Starting with release 0.70 (previously planned for 0.60 but pushed
        back), the XML parsing will switch to using the XML::LibXML module, in
        place of XML::Parser. You either do not have this module, or it is a
        version older than 1.58, the minimum version that will be
        required. When release 0.70 is made available, you will not be able to
        build this package without at least version 1.58 of XML::LibXML.
@@@@@

END
}

$cwd = cwd;

$CLEAN = 'pod2html-* *.html *.spec *.rpm rpmrc rpmmacro *.log t/*.log ' .
    'META.yml *.ppd ';

@scripts = (File::Spec->catfile(qw(etc make_method)));
$CLEAN  .= File::Spec->catfile(qw(methods *.xpl));

@PM_FILES = ();

find(sub { push(@PM_FILES, $File::Find::name) if (-f $_ and /\.pm$/) }, 'lib');

# Exclude Apache2 stuff until it's ready for deployment
@PM_FILES = grep(! /Apache2/, @PM_FILES);

%PM_FILES = map { ($temp = $_) =~ s|^lib|\$\(INST_LIB\)|; $_ => $temp }
    @PM_FILES;
# Handle the method code in "methods" specially:
find(sub {
         if (-f $_ and /\.base$/)
         {
             s/\.base$//;
             $PM_FILES{File::Spec->catfile('methods', "$_.xpl")} =
                 File::Spec->catfile('$(INST_LIB)', 'RPC', 'XML', "$_.xpl");
         }
     }, 'methods');
# Anything stuck under "lib" is more generic
find(sub {
         if (-f $_ and /\.base$/)
         {
             $File::Find::name =~ s/base$/xpl/;
             ($tmp = $File::Find::name) =~ s|^lib|\$(INST_LIB)|;
             $PM_FILES{$File::Find::name} = $tmp;
             $CLEAN .= " $File::Find::name";
         }
     }, 'lib');

WriteMakefile(
              NAME      => 'RPC::XML',
              VERSION   => '0.66',
              AUTHOR    => 'Randy J. Ray',
              ABSTRACT  => 'Data, client and server classes for XML-RPC',
              EXE_FILES => \@scripts,
              PM        => \%PM_FILES,
              PREREQ_PM => { XML::Parser  => 2.31,
                             File::Spec   => 0.8,
                             constant     => 1.03,
                             Scalar::Util => 1.20,
                             Test::More   => 0,
                             LWP          => 5.801 },
              dist      => { COMPRESS => 'gzip -9f' },
              clean     => { FILES => $CLEAN },
              LICENSE   => 'perl',
              SIGN      => 'here, please',
              MIN_PERL_VERSION => 5.006001,
             );

sub MY::post_initialize
{
    my $self = shift;

    my @text;
    my $makemeth = File::Spec->catfile(qw(etc make_method));

    push(@text,
         '.SUFFIXES: .xpl .base',
         '',
         '.base.xpl:',
         "\t\$(PERL) $makemeth --base=\$*",
         '');
    join("\n", @text);
}

sub MY::postamble
{
    my $self = shift;

    my @text;
    my $makemeth = File::Spec->catfile(qw(etc make_method));

    # Create the dependancy rules for the methods/XPL files
    for (sort grep(/\.xpl$/, keys %::PM_FILES))
    {
        s/\.xpl$//;
        push(@text, "$_.xpl: $_.base $_.help $_.code $makemeth");
    }

    join("\n", @text);
}
