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
#   $Id: Method.pm,v 1.3 2001/10/06 10:20:39 rjray Exp $
#
#   Description:    This class abstracts out all the method-related operations
#                   from the RPC::XML::Server class
#
#   Functions:      new
#                   clone
#                   is_valid
#                   name        \
#                   code         \
#                   signature     \ These are the accessor functions for the
#                   help          / data in the object, though it's visible.
#                   version      /
#                   hidden      /
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

require File::Spec;

$VERSION = do { my @r=(q$Revision: 1.3 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

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

    bless $data, $class;
    # This needs to happen post-bless in case of error (for error messages)
    $data->make_sig_table;
}

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

    bless $new_self, $self;
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

#
# These are basic accessor/setting functions for the various attributes
#
sub name      { $_[1] and $_[0]->{name}    = $_[1]; $_[0]->{name};    }
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
    ref $_[1] eq 'ARRAY' and $_[0]->{signature} = $_[1];
    # Return a copy of the array, not the original
    [ @{$_[0]->{signature}} ];
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

    my (%sigs, $one_sig, $tmp);

    %sigs = map { $_ => 1 } @{$self->{signature}};
    for $one_sig (@args)
    {
        $tmp = (ref $one_sig) ? join(' ', @$one_sig) : $one_sig;
        $sigs{$tmp} = 1;
    }
    $self->{signature} = [ keys %sigs ];

    $self;
}

sub delete_signature
{
    my $self = shift;
    my @args = @_;

    my (%sigs, $one_sig, $tmp);

    %sigs = map { $_ => 1 } @{$self->{signature}};
    for $one_sig (@args)
    {
        $tmp = (ref $one_sig) ? join(' ', @$one_sig) : $one_sig;
        delete $sigs{$tmp};
    }
    $self->{signature} = [ keys %sigs ];

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
        ($return, $rest) = split(/ /, $sig, 2);
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
