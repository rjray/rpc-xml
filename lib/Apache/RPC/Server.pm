###############################################################################
#
# This file copyright (c) 2001 by Randy J. Ray <rjray@blackperl.com>,
# all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License as distributed with Perl versions 5.005 and later. See
# http://language.perl.com/misc/Artistic.html
#
###############################################################################
#
#   $Id: Server.pm,v 1.1 2001/04/18 09:28:44 rjray Exp $
#
#   Description:    This package implements a RPC server as an Apache/mod_perl
#                   content handler. It uses the RPC::XML::Server package to
#                   handle request decoding and response encoding.
#
#   Functions:      handler
#
#   Libraries:      RPC::XML::Server
#
#   Global Consts:  $VERSION
#
###############################################################################

package Apache::RPC::Server;

use 5.005;
use strict;
use vars qw($VERSION);
use subs qw();

require RPC::XML::Server;

$VERSION = do { my @r=(q$Revision: 1.1 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

1;

###############################################################################
#
#   Sub Name:       handler
#
#   Description:    This is the default routine that Apache will look for
#                   when we set this class up as a content handler.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        Success:    1
#                   Failure:    0
#
###############################################################################
sub handler
{
}

__END__
