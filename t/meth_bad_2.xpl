<?xml version="1.0"?>
<!DOCTYPE methoddef SYSTEM "rpc-method.dtd">
<!--
    Generated automatically by make_method v1.06, Wed Nov 14 04:56:35 2001

    Any changes made here will be lost.
-->
<methoddef>
<name>system.identity</name>
<version>1.0</version>
<signature>string</signature>
<help>
Return the server name and version as a string
</help>
<code language="perl">
###############################################################################
#
#   Sub Name:       identity
#
#   Description:    Simply returns the server's identity as a string
#
#   Arguments:      First arg is server instance
#
#   Globals:        None.
#
#   Returns:        string
#
###############################################################################
sub identity
{
    use strict;

    sprintf('%s/%s', ref($_[0]), $_[0]-&gt;version);
}
</methoddef>
