###############################################################################
#
# This file copyright (c) 2001 by Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License as distributed with Perl versions 5.005 and later. See
# http://language.perl.com/misc/Artistic.html
#
###############################################################################
#
#   $Id: Method.pm,v 1.6 2001/11/30 11:54:38 rjray Exp $
#
#   Description:    This class abstracts out all the method-related operations
#                   from the RPC::XML::Server class
#
#   Functions:      new
#                   name        \
#                   code         \
#                   signature     \ These are the accessor functions for the
#                   help          / data in the object, though it's visible.
#                   version      /
#                   hidden      /
#                   clone
#                   is_valid
#                   add_signature
#                   delete_signature
#                   make_sig_table
#                   match_signature
#                   reload
#                   load_XPL_file
#
#   Libraries:      XML::Parser (used only on demand in load_XPL_file)
#                   File::Spec
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
use subs qw(new is_valid name code signature help version hidden
            add_signature delete_signature make_sig_table match_signature
            reload load_XPL_file);

use AutoLoader 'AUTOLOAD';
require File::Spec;

$VERSION = do { my @r=(q$Revision: 1.6 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

1;

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Create a new object of this class, storing the info on
#                   regular keys (no obfuscation used here).
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class to bless into
#                   @argz     in      variable  Disposition is variable; see
#                                                 below
#
#   Returns:        Success:    object ref
#                   Failure:    error string
#
###############################################################################
sub new
{
    my $class = shift;
    my @argz  = @_;

    my $data; # This will be a hashref that eventually gets blessed

    #
    # There are three things that @argz could be:
    #
    if (ref $argz[0])
    {
        # 1. A hashref containing all the relevant keys
        $data = {};
        %$data = %{$argz[0]};
    }
    elsif (@argz == 1)
    {
        # 2. Exactly one non-ref element, a file to load
        $data = load_XPL_file(undef, $argz[0]);
        return $data unless ref $data; # load_XPL_path signalled an error
    }
    else
    {
        # 3. If there is more than one arg, it's a sort-of-hash. That is, the
        #    key 'signature' is allowed to repeat.
        my ($key, $val);
        $data = {};
        $data->{signature} = [];
        while (@argz)
        {
            ($key, $val) = splice(@argz, 0, 2);
            if ($key eq 'signature')
            {
                # Since there may be more than one signature, we allow it to
                # repeat. Of course, that's also why we can't just take @argz
                # directly as a hash. *shrug*
                push(@{$data->{signature}},
                     [ ref($val) ? @$val : split(/ /, $val) ]);
            }
            elsif (exists $data->{$key})
            {
                return "${class}::new: Key '$key' may not be repeated";
            }
            else
            {
                $data->{$key} = $val;
            }
        }
    }

    return (ref($class) || $class) . '::new: Missing required data'
        unless (exists $data->{signature} and
                (ref($data->{signature}) eq 'ARRAY') and
                scalar(@{$data->{signature}}) and
                $data->{name} and $data->{code});
    bless $data, $class;
    # This needs to happen post-bless in case of error (for error messages)
    $data->make_sig_table;
}

#
# These are basic accessor/setting functions for the various attributes
#
sub name      { $_[0]->{name}; } # "name" cannot be changed at this level
sub help      { $_[1] and $_[0]->{help}    = $_[1]; $_[0]->{help};    }
sub version   { $_[1] and $_[0]->{version} = $_[1]; $_[0]->{version}; }
sub hidden    { $_[1] and $_[0]->{hidden}  = $_[1]; $_[0]->{hidden};  }
sub code
{
    ref $_[1] eq 'CODE' and $_[0]->{code} = $_[1];
    $_[0]->{code};
}
sub signature
{
    if ($_[1] and ref $_[1] eq 'ARRAY')
    {
        my $old = $_[0]->{signature};
        $_[0]->{signature} = $_[1];
        unless (ref($_[0]->make_sig_table))
        {
            # If it failed to re-init the table, restore the old list (and old
            # table). We don't have to check this return, since it had worked
            $_[0]->{signature} = $old;
            $_[0]->make_sig_table;
        }
    }
    # Return a copy of the array, not the original
    [ @{$_[0]->{signature}} ];
}

=pod

=head1 NAME

RPC::XML::Method - Object encapsulation of server-side RPC methods

=head1 SYNOPSIS

    require RPC::XML::Method;

    ...
    $method_1 = RPC::XML::Method->new({ name => 'system.identity',
                                        code => sub { ... },
                                        signature => [ 'string' ] });
    $method_2 = RPC::XML::Method->load_XPL_file('/path/to/status.xpl');

=head1 DESCRIPTION

The B<RPC::XML::Method> package is designed primarily for behind-the-scenes
use by the B<RPC::XML::Server> class and any subclasses of it. It is
documented here in case a project chooses to sub-class it for their purposes
(which would require setting the C<method_class> attribute when creating
server objects, see L<RPC::XML::Server>).

This package grew out of the increasing need to abstract the operations that
related to the methods a given server instance was providing. Previously,
methods were passed around simply as hash references. It was a small step then
to move them into a package and allow for operations directly on the objects
themselves. In the spirit of the original hashes, all the key data is kept in
clear, intuitive hash keys (rather than obfuscated as the other classes
do). Thus it is important to be clear on the interface here before
sub-classing this package.

=head1 USAGE

The following methods are provided by this class:

=over 4

=item new(FILE|HASHREF|LIST)

Creates a new object of the class, and returns a reference to it. The
arguments to the constructor are variable in nature, depending on the type:

=over 8

=item FILE

If there is exactly on argument that is not a reference, it is assumed to be a
filename from which the method is to be loaded. This is presumed to be in the
B<XPL> format descibed below (see L</"XPL File Structure">). If the file
cannot be opened, or if once opened cannot be parsed, an error is raised.

=item HASHREF

If there is exactly one argument that is a reference, it is assumed to be a
hash with the relevant information on the same keys as the object itself
uses. This is primarily to support backwards-compatibility to code written
when methods were implemented simply as hash references.

=item LIST

If there is more than one argument in the list, then the list is assumed to be
a sort of "ersatz" hash construct, in that one of the keys (C<signature>) is
allowed to occur multiple times. Otherwise, each of the following is allowed,
but may only occur once:

=over 12

=item name

The name of the method, as it will be presented to clients

=item code

A reference to a subroutine, or an anonymous subroutine, that will receive
calls for the method

=item signature

(May appear more than once) Provides one calling-signature for the method, as
either a space-separated string of types or a list-reference

=item help

The help-text for a method, which is generally used as a part of the
introspection interface for a server

=item version

The version number/string for the method

=item hidden

A boolean (true or false) value indicating whether the method should be hidden
from introspection and similar listings

=back

Note that all of these correspond to the values that can be changed via the
accessor methods detailed later.

=back

If any error occurs during object creation, an error message is returned in
lieu of the object reference.

=item clone

Create a copy of the calling object, and return the new reference. All
elements are copied over cleanly, except for the code reference stored on the
C<code> hash key. The clone will point to the same code reference as the
original. Elements such as C<signature> are copied, so that changes to the
clone will not impact the original.

=item name

Returns the name by which the server is advertising the method. Unlike the
next few accessors, this cannot be changed on an object. In order to
streamline the managment of methods within the server classes, this must
persist. However, the other elements may be used in the creation of a new
object, which may then be added to the server, if the name absolutely must
change.

=item code([NEW])

Returns or sets the code-reference that will receive calls as marshalled by
the server. The existing value is lost, so if it must be preserved, then it
should be retrieved prior to the new value being set.

=item signature([NEW])

Return a list reference containing the signatures, or set it. Each element of
the list is a string of space-separated types (the first of which is the
return type the method produces in that calling context). If this is being
used to set the signature, then an array reference must be passed that
contains one or more strings of this nature. Nested list references are not
allowed at this level. If the new signatures would cause a conflict (a case in
which the same set of input types are specified for different output types),
the old set is silently restored.

=item help([NEW])

Returns or sets the help-text for the method. As with B<code>, the previous
value is lost.

=item hidden([NEW])

Returns or sets the hidden status of the method. Setting it loses the previous
value.

=item version([NEW])

Returns or sets the version string for the method (overwriting as with the
other accessors).

=item is_valid

Returns a true/false value as to whether the object currently has enough
content to be a valid method for a server to publish. This entails having at
the very least a name, one or more signatures, and a code-reference to route
the calls to. A server created from the classes in this software suite will
not accept a method that is not valid.

=item add_signature(LIST)

Add one or more signatures (which may be a list reference or a string) to the
internal tables for this method. Duplicate signatures are ignored. If the new
signature would cause a conflict (a case in which the same set of input types
are specified for different output types), the old set is restored and an
error message is returned.

=item delete_signature(LIST)

Deletes the signature or signatures (list reference or string) from the
internal tables. Quietly ignores any signature that does not exist. If the new
signature would cause a conflict (a case in which the same set of input types
are specified for different output types), the old set is restored and an
error message is returned.

=item match_signature(SIGNATURE)

Check that the passed-in signature is known to the method, and if so returns
the type that the method should be returning as a result of the call. Returns
a zero (0) otherwise. This differs from other signature operations in that the
passed-in signature (which may be a list-reference or a string) B<I<does not
include the return type>>. This method is provided so that servers may check a
list of arguments against type when marshalling an incoming call. For example,
a signature of C<'int int'> would be tested for by calling
C<$M-E<gt>match_signature('int')> and expecting the return value to be C<int>.

=item reload

Instruct the object to reload itself from the file it originally was loaded
from, assuming that it was loaded from a file to begin with. Returns an error
if the method was not originally loaded from a file, or if an error occurs
during the reloading operation.

=back

=head2 Additional Hash Data

In addition to the attributes managed by the accessors documented earlier, the
following hash keys are also available for use. These are also not strongly
protected, and the same care should be taken before altering any of them:

=over 4

=item C<file>

When the method was loaded from a file, this key contains the path to the file
used.

=item C<mtime>

When the method was loaded from a file, this key contains the
modification-time of the file, as a UNIX-style C<time> value. This is used to
check for changes to the file the code was originally read from.

=item C<called>

When the method is being used by one of the server classes provided in this
software suite, this key is incremented each time the server object dispatches
a request to the method. This can later be checked to provide some indication
of how frequently the method is being invoked.

=back

=head2 XPL File Structure

This section focuses on the way in which methods are expressed in these files,
referred to here as "XPL files" due to the C<*.xpl> filename extension
(which stands for "XML Procedure Layout"). This mini-dialect, based on XML,
is meant to provide a simple means of specifying method definitions separate
from the code that comprises the application itself. Thus, methods may
theoretically be added, removed, debugged or even changed entirely without
requiring that the server application itself be rebuilt (or, possibly, without
it even being restarted).

=head3 The XPL file structure

The B<XPL Procedure Layout> dialect is a very simple application of XML to the
problem of expressing the method in such a way that it could be useful to
other packages than this one, or useful in other contexts than this one.

The lightweight DTD for the layout can be summarized as:

        <!ELEMENT  methoddef  (name, version?, hidden?, signature+,
                               help?, code)>
        <!ELEMENT  name       (#PCDATA)>
        <!ELEMENT  version    (#PCDATA)>
        <!ELEMENT  hidden     EMPTY>
        <!ELEMENT  signature  (#PCDATA)>
        <!ELEMENT  help       (#PCDATA)>
        <!ELEMENT  code       (#PCDATA)>
        <!ATTLIST  code       language (#PCDATA)>

The containing tag is always C<E<lt>methoddefE<gt>>. The tags that specify
name, signatures and the code itself must always be present. Some optional
information may also be supplied. The "help" text, or what an introspection
API would expect to use to document the method, is also marked as optional.
Having some degree of documentation for all the methods a server provides is
a good rule of thumb, however.

The default methods that this package provides are turned into XPL files by
the B<make_method> tool (described later). The final forms of these may serve
as direct examples of what the file should look like.

=head3 Information used only for book-keeping

Some of the information in the XPL file is only for book-keeping: the version
stamp of a method is never involved in the invocation. The server also keeps
track of the last-modified time of the file the method is read from, as well
as the full directory path to that file. The C<E<lt>hidden /E<gt>> tag is used
to identify those methods that should not be exposed to the outside world
through any sort of introspection/documentation API. They are still available
and callable, but the client must possess the interface information in order
to do so.

=head3 The information crucial to the method

The name, signatures and code must be present for obvious reasons. The
C<E<lt>nameE<gt>> tag tells the server what external name this procedure is
known by. The C<E<lt>signatureE<gt>> tag, which may appear more than once,
provides the definition of the interface to the function in terms of what
types and quantity of arguments it will accept, and for a given set of
arguments what the type of the returned value is. Lastly is the
C<E<lt>codeE<gt>> tag, without which there is no procedure to remotely call.

=head3 Why the <code> tag allows multiple languages

Note that the C<E<lt>codeE<gt>> tag is the only one with an attribute, in this
case "language". This is designed to allow for one XPL file to provide a given
method in multiple languages. Why, one might ask, would there be a need for
this?

It is the hope behind this package that collections of RPC suites may one day
be made available as separate entities from this specific software package.
Given this hope, it is not unreasonable to suggest that such a suite of code
might be implemented in more than one language (each of Perl, Python, Ruby and
Tcl, for example). Languages which all support the means by which to take new
code and add it to a running process on demand (usually through an "C<eval>"
keyword or something similar). If the file F<A.xpl> is provided with
implementations in all four of the above languages, the name, help text,
signature and even hidden status would likely be identical. So, why not share
the non-language-specific elements in the spirit of re-use?

=head3 The "make_method" utility

The utility script C<make_method> is provided as a part of this software
suite. It allows for the automatic creation of XPL files from either
command-line information or from template files. It has a wide variety of
features and options, and is out of the scope of this particular manual
page. The package F<Makefile.PL> features an example of engineering the
automatic generation of XPL files and their delivery as a part of the normal
Perl module build process. Using this tool is highly recommended over managing
XPL files directly. For the full details, see L<make_method>.

=head1 DIAGNOSTICS

Unless otherwise noted in the individual documentation sections, all methods
return the object reference on success, or a (non-reference) text string
containing the error message upon failure.

=head1 CAVEATS

Moving the method management to a separate class adds a good deal of overhead
to the general system. The trade-off in reduced complexity and added
maintainability should offset this.

=head1 LICENSE

This module is licensed under the terms of the Artistic License that covers
Perl. See <http://language.perl.com/misc/Artistic.html> for the
license.

=head1 SEE ALSO

L<RPC::XML::Server>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut

__END__

###############################################################################
#
#   Sub Name:       clone
#
#   Description:    Create a near-exact copy of the invoking object, save that
#                   the listref in the "signature" key is a copy, not a ref
#                   to the same list.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#
#   Returns:        Success:    $new_self
#                   Failure:    error message
#
###############################################################################
sub clone
{
    my $self = shift;

    my $new_self = {};
    for (keys %$self)
    {
        next if $_ eq 'signature';
        $new_self->{$_} = $self->{$_};
    }
    $new_self->{signature} = [];
    @{$new_self->{signature}} = @{$self->{signature}};

    bless $new_self, ref($self);
}

###############################################################################
#
#   Sub Name:       is_valid
#
#   Description:    Boolean test to tell if the calling object has sufficient
#                   data to be used as a server method for RPC::XML::Server or
#                   Apache::RPC::Server.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object to test
#
#   Returns:        Success:    1, valid/complete
#                   Failure:    0, invalid/incomplete
#
###############################################################################
sub is_valid
{
    my $self = shift;

    return ((ref($self->{code}) eq 'CODE') and $self->{name} and
            (ref($self->{signature}) && scalar(@{$self->{signature}})));
}

###############################################################################
#
#   Sub Name:       add_signature
#                   delete_signature
#
#   Description:    This pair of functions may be used to add and remove
#                   signatures from a method-object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   @args     in      list      One or more signatures
#
#   Returns:        Success:    $self
#                   Failure:    error string
#
###############################################################################
sub add_signature
{
    my $self = shift;
    my @args = @_;

    my (%sigs, $one_sig, $tmp, $old);

    # Preserve the original in case adding the new one causes a problem
    $old = $self->{signature};
    %sigs = map { $_ => 1 } @{$self->{signature}};
    for $one_sig (@args)
    {
        $tmp = (ref $one_sig) ? join(' ', @$one_sig) : $one_sig;
        $sigs{$tmp} = 1;
    }
    $self->{signature} = [ keys %sigs ];
    unless (ref($tmp = $self->make_sig_table))
    {
        # Because this failed, we have to restore the old table and return
        # an error
        $self->{signature} = $old;
        $self->make_sig_table;
        return ref($self) . '::add_signature: Error re-hashing table: ' .
            $tmp;
    }

    $self;
}

sub delete_signature
{
    my $self = shift;
    my @args = @_;

    my (%sigs, $one_sig, $tmp, $old);

    # Preserve the original in case adding the new one causes a problem
    $old = $self->{signature};
    %sigs = map { $_ => 1 } @{$self->{signature}};
    for $one_sig (@args)
    {
        $tmp = (ref $one_sig) ? join(' ', @$one_sig) : $one_sig;
        delete $sigs{$tmp};
    }
    $self->{signature} = [ keys %sigs ];
    unless (ref($tmp = $self->make_sig_table))
    {
        # Because this failed, we have to restore the old table and return
        # an error
        $self->{signature} = $old;
        $self->make_sig_table;
        return ref($self) . '::delete_signature: Error re-hashing table: ' .
            $tmp;
    }

    $self;
}

###############################################################################
#
#   Sub Name:       make_sig_table
#
#   Description:    Create a hash table of the signatures that maps to the
#                   corresponding return type for that particular invocation.
#                   Makes looking up call patterns much easier.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#
#   Returns:        Success:    $self
#                   Failure:    error message
#
###############################################################################
sub make_sig_table
{
    my $self = shift;

    my ($sig, $return, $rest);

    delete $self->{sig_table};
    for $sig (@{$self->{signature}})
    {
        ($return, $rest) = split(/ /, $sig, 2); $rest = '' unless $rest;
        # If the key $rest already exists, then this is a collision
        return ref($self) . '::make_sig_table: Cannot have two different ' .
            "return values for one set of params ($return vs. " .
            "$self->{sig_table}->{$rest})"
                if $self->{sig_table}->{$rest};
        $self->{sig_table}->{$rest} = $return;
    }

    $self;
}

###############################################################################
#
#   Sub Name:       match_signature
#
#   Description:    Determine if the passed-in signature string matches any
#                   of this method's known signatures.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $sig      in      scalar    Signature to check for
#
#   Returns:        Success:    return type as a string
#                   Failure:    0
#
###############################################################################
sub match_signature
{
    my $self = shift;
    my $sig  = shift;

    $sig = join(' ', @$sig) if ref $sig;

    return $self->{sig_table}->{$sig} || 0;
}

###############################################################################
#
#   Sub Name:       reload
#
#   Description:    Reload the method's code and ancillary data from the file
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#
#   Returns:        Success:    $self
#                   Failure:    error message
#
###############################################################################
sub reload
{
    my $self = shift;

    return ref($self) . '::reload: No file associated with method ' .
        $self->{name} unless $self->{file};
    my $tmp = $self->load_XPL_file($self->{file});

    # Re-calculate the signature table, in case that changed as well
    return (ref $tmp) ? $self->make_sig_table : $tmp;
}

###############################################################################
#
#   Sub Name:       load_XPL_file
#
#   Description:    Load a XML-encoded method description (generally denoted
#                   by a *.xpl suffix) and return the relevant information.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $file     in      scalar    File to load
#
#   Returns:        Success:    hashref of values
#                   Failure:    error string
#
###############################################################################
sub load_XPL_file
{
    my $self = shift;
    my $file = shift;

    require XML::Parser;

    my ($me, $signature, $code, $codetext, $accum, $P, %attr);
    local *F;

    $me = (ref $self) ? ref($self) : __PACKAGE__;
    $me .= '::load_XPL_file';
    $self = {} unless ref $self;
    # So these don't end up undef, since they're optional elements
    $self->{hidden} = 0; $self->{version} = ''; $self->{help} = '';
    $self->{called} = 0; $self->{signature} = [];
    open(F, "< $file");
    return "$me: Error opening $file for reading: $!" if ($?);
    $P = XML::Parser
        ->new(Handlers => {Char  => sub { $accum .= $_[1] },
                           Start => sub { %attr = splice(@_, 2) },
                           End   =>
                           sub {
                               my $elem = $_[1];

                               $accum =~ s/^[\s\n]+//;
                               $accum =~ s/[\s\n]+$//;
                               if ($elem eq 'signature')
                               {
                                   push(@{$self->{signature}}, $accum);
                               }
                               elsif ($elem eq 'code')
                               {
                                   $self->{$elem} = $accum
                                       unless ($attr{language} and
                                               $attr{language} ne 'perl');
                               }
                               else
                               {
                                   $self->{$elem} = $accum;
                               }

                               %attr = ();
                               $accum = '';
                           }});
    return "$me: Error creating XML::Parser object" unless $P;
    # Trap any errors
    eval { $P->parse(*F) };
    return "$me: Error parsing $file: $@" if $@;

    # Try to normalize $codetext before passing it to eval
    ($codetext = $self->{code}) =~
        s/sub[\s\n]+[\w:]+[\s\n]+\{/\$code = sub \{/;
    eval "$codetext";
    return "$me: Error creating anonymous sub: $@" if $@;

    $self->{code} = $code;
    # The XML::Parser approach above gave us an empty "methoddef" key
    delete $self->{methoddef};
    # Add the file's mtime for when we check for stat-based reloading
    $self->{mtime} = (stat $file)[9];
    $self->{file} = $file;

    $self;
}
