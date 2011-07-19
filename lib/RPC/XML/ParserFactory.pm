###############################################################################
#
# This file copyright (c) 2001-2011 Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License 2.0 (http://www.opensource.org/licenses/artistic-license-2.0.php) or
# the GNU LGPL (http://www.opensource.org/licenses/lgpl-2.1.php).
#
###############################################################################
#
#   Description:    This is the RPC::XML::ParserFactory class, a factory for
#                   classes that derive from the RPC::XML::Parser class.
#
#   Functions:      import
#                   new
#                   register
#
#   Libraries:      RPC::XML::Parser::XMLParser \
#                   RPC::XML::Parser::XMLLibXML  > One (or more) of these
#                   RPC::XML::Parser::XMLSAX    /
#
#   Global Consts:  $VERSION
#
#   Environment:    None.
#
###############################################################################

package RPC::XML::ParserFactory;

use 5.008008;
use strict;
use warnings;
use vars qw($VERSION %AVAILABLE $PARSER_CLASS);
use subs qw(import new register);

# Because this is a factory class, there are some eval's that violate this
# critic policy, but can't be worked around:
## no critic (RequireCheckingReturnValueOfEval)

$VERSION = '1.03';
$VERSION = eval $VERSION; ## no critic (ProhibitStringyEval)

# These are the known parsers supported, not including any that are specified
# by the user at import-time.
$PARSER_CLASS = 'XML::Parser';
%AVAILABLE    = (
    'XML::Parser' => 'RPC::XML::Parser::XMLParser',
    'XML::LibXML' => 'RPC::XML::Parser::XMLLibXML',
);

# "Normalize" the key-names to allow some simplicity (and sugar):
for (keys %AVAILABLE)
{
    my $key = lc $_;
    $AVAILABLE{$key} = $AVAILABLE{$_};
    $key =~ s/:://g;
    $AVAILABLE{$key} = $AVAILABLE{$_};
}

###############################################################################
#
#   Sub Name:       import
#
#   Description:    Method called when this module is use'd
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class name (not used)
#                   @args     in      list      Arguments to the import
#
#   Globals:        $PARSER_CLASS
#
#   Returns:        void
#
###############################################################################
sub import
{
    my (undef, @args) = @_;

    # As a special-case, this one parameter might be specified without the
    # key, if it is the ONLY thing passed:
    if (1 == @args)
    {
        @args = (class => @args);
    }

    # For now, the only arguments are key/value pairs so it's safe to coerce
    # this into a hash
    my %argz = @args;

    # In fact, for now, this is the only argument:
    if ($argz{class})
    {
        $PARSER_CLASS = $argz{class};
    }

    return;
}

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Constructor. Save any important attributes, leave the
#                   heavy lifting for the parse() routine and XML::Parser.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class we're initializing
#                   %attr     in      hash      Any extras the caller wants
#
#   Globals:        $RPC::XML::ERROR
#
#   Returns:        Success:    object ref
#                   Failure:    undef
#
###############################################################################
sub new
{
    my ($class, %attrs) = @_;

    my $factory = delete $attrs{class} || $PARSER_CLASS;

    if ($class = $AVAILABLE{$factory})
    {
        eval "require $class;"; ## no critic (ProhibitStringyEval)
        if ($@)
        {
            $RPC::XML::ERROR = __PACKAGE__ . "::new: Error loading $class (" .
              "factory for '$factory'): $@";
            return;
        }
    }
    else
    {
        # This means that the class is not one of the built-in ones. Try to
        # load it, then make sure it's a sub-class of this one:
        $class = $factory;
        eval "require $class;"; ## no critic (ProhibitStringyEval)
        if ($@)
        {
            $RPC::XML::ERROR = __PACKAGE__ . "::new: Error loading $class: $@";
            return;
        }
        # Loaded OK... is it a descendent?
        if  (! $class->isa(__PACKAGE__))
        {
            $RPC::XML::ERROR = __PACKAGE__ . "::new: Class '$class' cannot " .
              'be used, as it is not a sub-class of ' . __PACKAGE__;
            return;
        }
    }

    return $class->new(%attrs);
}

1;

__END__

=head1 NAME

RPC::XML::ParserFactory - A factory class for RPC::XML::Parser objects

=head1 SYNOPSIS

    use RPC::XML::ParserFactory;
    ...
    $P = RPC::XML::ParserFactory->new();
    $P->parse($message);

=head1 DESCRIPTION

The B<RPC::XML::ParserFactory> class encapsulates the process of creating
parser objects that adhere to the interface described in
L<RPC::XML::Parser|RPC::XML::Parser>.  Under the hood, the parser object
created and returned could be from any of a number of implementation classes.

=head1 IMPORT-TIME ARGUMENTS

You can specify a particular underlying parser class to use, if you do not
want B<RPC::XML::ParserFactory> to use the default class. This is done with
the C<class> keyword:

    use RPC::XML::ParserFactory (class => 'XML::Parser');

The value may be the name for any of the built-in classes, or it may be the
name of a class that inherits from B<RPC::XML::Parser> (and can thus be
"manufactured" by the factory). The value is saved and becomes the default
class for any calls to B<new> that do not explicitly name a class to use.

Note that if the specified class is not valid, this is not tested until the
first call to B<new>, at which point an invalid class will cause an exception
(error) to occur. The constructor will return C<undef> and the
B<$RPC::XML::ERROR> variable will contain the error message.

=head2 Names of Built-In Parsers

The following names are valid when specified as the value of the C<class>
argument described above:

=over 4

=item XML::Parser

=item xml::parser

=item xmlparser

All of these specify the parser implementation based on the B<XML::Parser>
module. This is the default parser if the user does not specify any
alternative.

=item XML::LibXML

=item xml::libxml

=item xmllibxml

These specify a parser implementation based on the B<XML::LibXML> module.
This is a new parser and not as well-vetted as the previous one, hence it
must be explicitly requested.

=back

=head1 SUBROUTINES/METHODS

The methods are:

=over 4

=item new([ARGS])

Create a new instance of the class. Any extra data passed to the constructor
is taken as key/value pairs (B<not> a hash reference) and attached to the
object.

This method passes all arguments on to the new() method of the chosen
implementation class, except for the following:

=over 4

=item class NAME

If the user chooses, they may specify an explicit class to use for parsers
when calling new(). If passed, this overrides any value that was given at
use-time (processed by import()).

=back

=back

=head1 DIAGNOSTICS

The constructor returns C<undef> upon failure, with the error message available
in the global variable B<C<$RPC::XML::ERROR>>.

=head1 BUGS

Please report any bugs or feature requests to
C<bug-rpc-xml at rt.cpan.org>, or through the web interface at
L<http://rt.cpan.org/NoAuth/ReportBug.html?Queue=RPC-XML>. I will be
notified, and then you'll automatically be notified of progress on
your bug as I make changes.

=head1 SUPPORT

=over 4

=item * RT: CPAN's request tracker

L<http://rt.cpan.org/NoAuth/Bugs.html?Dist=RPC-XML>

=item * AnnoCPAN: Annotated CPAN documentation

L<http://annocpan.org/dist/RPC-XML>

=item * CPAN Ratings

L<http://cpanratings.perl.org/d/RPC-XML>

=item * Search CPAN

L<http://search.cpan.org/dist/RPC-XML>

=item * MetaCPAN

L<https://metacpan.org/release/RPC-XML>

=item * Source code on GitHub

L<http://github.com/rjray/rpc-xml>

=back

=head1 LICENSE AND COPYRIGHT

This file and the code within are copyright (c) 2011 by Randy J. Ray.

Copying and distribution are permitted under the terms of the Artistic
License 2.0 (L<http://www.opensource.org/licenses/artistic-license-2.0.php>) or
the GNU LGPL 2.1 (L<http://www.opensource.org/licenses/lgpl-2.1.php>).

=head1 CREDITS

The B<XML-RPC> standard is Copyright (c) 1998-2001, UserLand Software, Inc.
See L<http://www.xmlrpc.com> for more information about the B<XML-RPC>
specification.

=head1 SEE ALSO

L<RPC::XML|RPC::XML>, L<RPC::XML::Client|RPC::XML::Client>,
L<RPC::XML::Server|RPC::XML::Server>, L<XML::Parser|XML::Parser>

=head1 AUTHOR

Randy J. Ray C<< <rjray@blackperl.com> >>

=cut
