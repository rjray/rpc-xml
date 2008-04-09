###############################################################################
#
# This file copyright (c) 2001-2008 Randy J. Ray, all rights reserved
#
# See "LICENSE" in the documentation for licensing and redistribution terms.
#
###############################################################################
#
#   $Id$
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

use 5.005;
use strict;
use vars qw($VERSION);

require RPC::XML::Procedure;

@RPC::XML::Method::ISA = qw(RPC::XML::Procedure);
$VERSION = '1.09';

1;

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
sub-class of B<RPC::XML::Procedure>. Please see L<RPC::XML::Procedure> for
details on the methods and usage.

By the time of 1.0 release of this software package, this file will be removed
completely.

=head1 LICENSE

This module and the code within are released under the terms of the Artistic
License 2.0
(http://www.opensource.org/licenses/artistic-license-2.0.php). This code may
be redistributed under either the Artistic License or the GNU Lesser General
Public License (LGPL) version 2.1
(http://www.opensource.org/licenses/lgpl-license.php).

=head1 SEE ALSO

L<RPC::XML::Procedure>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut

__END__
