###############################################################################
#
# $Id: Makefile.PL,v 1.19 2002/03/23 06:21:39 rjray Exp $
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

$cwd = cwd;

$CLEAN = 'pod2html-* *.html *.spec *.rpm rpmrc rpmmacro *.log t/*.log ';

@scripts = (File::Spec->catfile(qw(etc make_method)));
$CLEAN  .= File::Spec->catfile(qw(methods *.xpl));

@PM_FILES = ();

find(sub { push(@PM_FILES, $File::Find::name) if (-f $_ and /\.pm$/) }, 'lib');

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
              VERSION   => '0.37',
              EXE_FILES => \@scripts,
              PM        => \%PM_FILES,
              PREREQ_PM => { XML::Parser => 2.25,
                             File::Spec  => 0.8,
                             LWP         => 0 },
              dist      => { COMPRESS => 'gzip -9f' },
              clean     => { FILES => $CLEAN },
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

sub MY::tools_other
{
    package MY;

    my $text = shift->SUPER::tools_other(@_);

    "$text\nECHO=\@echo\nRPM=rpm\n";
}

sub MY::post_constants
{
    my $self = shift;

    "SPECFILE=perl-$self->{DISTNAME}.spec";
}

sub MY::postamble
{
    my $self = shift;

    my @text;

    #
    # Generate the spec file from <DATA> with substitutions
    #
    print "Generating perl-$self->{DISTNAME}.spec...\n";
    $self->{RELEASE} = 1 unless $self->{RELEASE};
    $self->{INSTALLDIR} = $Config{installsitelib} unless $self->{INSTALLDIR};
    open(SPECI, "< spec.in");
    if ($?)
    {
        die "Error opening template spec file: $!, stopped";
    }
    open(SPECO, "> perl-$self->{DISTNAME}.spec");
    if ($?)
    {
        die "Error creating output spec file: $!, stopped";
    }

    for (<SPECI>)
    {
        s/(\$\{(\w+)\})/$self->{$2}/eg;
        print SPECO $_;
    }
    close(SPECO); close(SPECI);

    print "Generating rpmrc...\n";
    open(RPMRC, "> rpmrc");
    if ($?)
    {
        die "Error creating rpmrc file: $!, stopped";
    }
    else
    {
        print RPMRC <<END_rpmrc;
include: /usr/lib/rpm/rpmrc
macrofiles: /usr/lib/rpm/macros:rpmmacro
END_rpmrc
        close(RPMRC);
    }
    print "Generating rpmmacros...\n";
    open(MACRO, "> rpmmacro");
    if ($?)
    {
        die "Error creating macro file: $!, stopped";
    }
    else
    {
        print MACRO <<END_macro;
%_rpmfilename           %%{NAME}-%%{VERSION}-%%{RELEASE}.%%{ARCH}.rpm
%_builddir              %{_topdir}
%_sourcedir             %{_topdir}
%_specdir               %{_topdir}
%_srcrpmdir             %{_topdir}
%_rpmdir                %{_topdir}
%_topdir                $cwd
END_macro
        close(MACRO);
    }

    # Create the rules that create RPM and SRPM files
    push(@text, qq{
rpm: \$(SPECFILE) \$(DISTVNAME).tar\$(SUFFIX)
\t\$(RPM) -bb --clean --rcfile rpmrc \$(SPECFILE)

srpm: \$(SPECFILE) \$(DISTVNAME).tar\$(SUFFIX)
\t\$(RPM) -bs --clean --rcfile rpmrc \$(SPECFILE)
});

    # Create the dependancy rules for the methods/XPL files
    for (sort grep(/\.xpl$/, keys %::PM_FILES))
    {
        s/\.xpl$//;
        push(@text, "$_.xpl: $_.base $_.help $_.code");
    }

    join("\n", @text);
}
