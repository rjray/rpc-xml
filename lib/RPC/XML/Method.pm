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
#   Description:    This is now an empty sub-class of RPC::XML::Procedure.
#                   It is given its own file to allow for a minimal manual
#                   page redirecting people to the newer class.
#
#   Functions:      None.
#
#   Libraries:      RPC::XML::Procedure
#
#   Global Consts:  $VERSION
#
#   Environment:    None.
#
###############################################################################

package RPC::XML::Method;

use 5.006001;
use strict;
use warnings;
use vars qw($VERSION);
use base qw(RPC::XML::Procedure);

$VERSION = '1.14';
$VERSION = eval $VERSION; ## no critic (ProhibitStringyEval)

1;

# This is now a skeleton, so it doesn't have a full POD section
## no critic (Documentation::RequirePodSections)

__END__

=head1 NAME

RPC::XML::Method - Object encapsulation of server-side RPC methods

=head1 SYNOPSIS

    require RPC::XML::Method;

    ...
    $method_1 = RPC::XML::Method->new({ name => 'system.identity',
                                        code => sub { ... },
                                        signature => [ 'string' ] });
    $method_2 = RPC::XML::Method->new('/path/to/status.xpl');

=head1 DESCRIPTION

This package is no longer a distinct, separate entity. It has become an empty
sub-class of B<RPC::XML::Procedure>. Please see
L<RPC::XML::Procedure|RPC::XML::Procedure> for details on the methods and
usage.

By the time of 1.0 release of this software package, this file will be removed
completely.

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

=item * Source code on GitHub

L<http://github.com/rjray/rpc-xml>

=back

=head1 LICENSE AND COPYRIGHT

This file and the code within are copyright (c) 2010 by Randy J. Ray.

Copying and distribution are permitted under the terms of the Artistic
License 2.0 (L<http://www.opensource.org/licenses/artistic-license-2.0.php>) or
the GNU LGPL 2.1 (L<http://www.opensource.org/licenses/lgpl-2.1.php>).

=head1 CREDITS

The B<XML-RPC> standard is Copyright (c) 1998-2001, UserLand Software, Inc.
See <http://www.xmlrpc.com> for more information about the B<XML-RPC>
specification.

=head1 SEE ALSO

L<RPC::XML::Procedure|RPC::XML::Procedure>

=head1 AUTHOR

Randy J. Ray C<< <rjray@blackperl.com> >>
