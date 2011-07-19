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
#   Description:    This is the RPC::XML::Parser class, an empty class that
#                   acts as an interface for parser implementations that can
#                   be created/returned by RPC::XML::ParserFactory.
#
#   Functions:      new
#                   parse
#
#   Global Consts:  $VERSION
#
#   Environment:    None.
#
###############################################################################

package RPC::XML::Parser;

use 5.008008;
use strict;
use warnings;
use vars qw($VERSION);
use subs qw(new parse);

$VERSION = '1.24';
$VERSION = eval $VERSION; ## no critic (ProhibitStringyEval)

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Constructor. Dies, because this should be overridden.
#
#                   Per RT#50013: Now, when called specifically for this class,
#                   quietly loads RPC::XML::ParserFactory and instantiates a
#                   parser based on XML::Parser.
#
#   Returns:        undef
#
###############################################################################
sub new
{
    my ($class, @args) = @_;

    if ($class eq 'RPC::XML::Parser')
    {
        # For sake of not breaking backwards-compatibility with projects like
        # Catalyst::Plugin::Server::XMLRPC, in this case quietly load the
        # RPC::XML::ParserFactory and return a factory-generated instance:
        require RPC::XML::ParserFactory;

        return RPC::XML::ParserFactory->new(class => 'xmlparser', @args);
    }

    die __PACKAGE__ . '::new: This method should have been overridden by ' .
        "the $class class\n";
}

###############################################################################
#
#   Sub Name:       parse
#
#   Description:    Parse the requested string or stream, or return a
#                   push-parser instance. In this case, it dies because the
#                   sub-class should have overridden it.
#
#   Returns:        dies
#
###############################################################################
sub parse
{
    my $class = shift;
    $class = ref($class) || $class;

    die __PACKAGE__ . '::parse: This method should have been overridden by ' .
        "the $class class\n";
}

###############################################################################
#
#   Sub Name:       parse_more
#
#   Description:    When called on a push-parser instance (which may or may
#                   not be the same class), parses additional content and
#                   waits for more. In this case it dies because the sub-class
#                   should have overridden it.
#
#   Returns:        dies
#
###############################################################################
sub parse_more
{
    my $class = shift;
    $class = ref($class) || $class;

    die __PACKAGE__ . '::parse_more: This method should have been overridden' .
        " by the $class class\n";
}

###############################################################################
#
#   Sub Name:       parse_done
#
#   Description:    When called on a push-parser instance (which may or may
#                   not be the same class), finishes the parse process and
#                   returns the result. In this case it dies because the
#                   sub-class should have overridden it.
#
#   Returns:        dies
#
###############################################################################
sub parse_done
{
    my $class = shift;
    $class = ref($class) || $class;

    die __PACKAGE__ . '::parse_done: This method should have been overridden' .
        " by the $class class\n";
}

1;

__END__

=head1 NAME

RPC::XML::Parser - Interface for parsers created by RPC::XML::ParserFactory

=head1 SYNOPSIS

This class is not instantiated directly; see
L<RPC::XML::ParserFactory|RPC::XML::ParserFactory>.

=head1 DESCRIPTION

The B<RPC::XML::Parser> class encapsulates the interface for the parsing
process. It is an empty class that is used in conjuntion with the
B<RPC::XML::ParserFactory> class.

All parser implementations that are intended to be returned by calls to
RPC::XML::ParserFactory::new() should declare this as their parent class.

=head1 SUBROUTINES/METHODS

This class provides empty implementations for the following methods. A parser
implementation must provide definitions for B<both> of these methods. If the
versions from this class are triggered they will throw exceptions (C<die>).

The descriptions below define the interface that implementations must
adhere to.

=over 4

=item new([ARGS])

Create a new instance of the class. Any extra data passed to the constructor
is taken as key/value pairs (B<not> a hash reference) and attached to the
object.

The following parameters are currently recognized:

=over 8

=item base64_to_fh

If passed with a true value, this tells the parser that incoming Base64 data
is to be spooled to a filehandle opened onto an anonymous temporary file. The
file itself is unlinked after opening, though the resulting B<RPC::XML::base64>
object can use its C<to_file> method to save the data to a specific file at a
later point. No checks on size are made; if this option is set, B<all> Base64
data goes to filehandles.

=item base64_temp_dir

If this argument is passed, the value is taken as the directory under which
the temporary files are created. This is so that the application is not locked
in to the list of directories that B<File::Spec> defaults to with its
C<tmpdir> method. If this is not passed, the previously-mentioned method is
used to derive the directory in which to create the temporary files. Only
relevant if B<base64_to_fh> is set.

=back

The C<base64*> parameters do not have to be implemented if the user has
no plans to use the C<to_file> method of the B<RPC::XML::base64> data-class.

As a special-case, to preserve backwards compatibility with pre-0.69 versions
of this package, new() has special behavior when specifically called for the
package B<RPC::XML::Parser>. When called for this package, the constructor
quietly loads B<RPC::XML::ParserFactory> and uses it to construct and return
an instance of a parser based on B<XML::Parser>.

=item parse [ STRING | STREAM ]

Parse the XML document specified in either a string or a stream. The stream
may be any file descriptor, derivative of B<IO::Handle>, etc.

The value returned must be one of the following:

=over 4

=item RPC::XML::request instance

When passed a valid XML-RPC request message, the return value should be
an instance of the B<RPC::XML::request> class.

=item RPC::XML::response instance

Likewise, when passed a valid XML-RPC response, the return value should be
an instance of the B<RPC::XML::response> class.

=item string containing an error message

If the message does not conform to either a request or a response, or does
not properly parse, the return value must be a string containing the error
message.

=item A non-blocking (push) parser instance

If no arguments are passed in, the return value must be a parser object that
implements push-parsing (non-blocking). It does not have to be of the same
class as the original object, but it must support the remaining two methods

=back

=back

The next two methods are only called on push-parser instances, and as such do
not have to be implemented by the actual factory-compatible parser. It is
enough if the non-blocking parser instance it returns from the no-argument call
to parse() implements these:

=over 4

=item parse_more STRING

Send a chunk of the current XML document to the parser for processing.

=item parse_done

Signal the end of parsing. The return value from this should be one of the
same three possibilities that the direct use of parse() (above) returns:

=over 4

=item RPC::XML::request instance

=item RPC::XML::response instance

=item string containing an error message

=back

parse_done() may also signal an error by throwing an exception.

=back

=head1 DIAGNOSTICS

Unless otherwises specified, routines return the object reference itself upon
a successful operation, and an error string (which is not a blessed reference)
upon error.

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
See <http://www.xmlrpc.com> for more information about the B<XML-RPC>
specification.

=head1 SEE ALSO

L<RPC::XML|RPC::XML>, L<RPC::XML::ParserFactory|RPC::XML::ParserFactory>,
L<RPC::XML::Parser::XMLParser|RPC::XML::Parser::XMLParser>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut
