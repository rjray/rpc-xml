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
#   $Id: ServerMethod.pm,v 1.1 2001/08/16 10:19:19 rjray Exp $
#
#   Description:    This class abstracts out all the method-related operations
#                   from the RPC::XML::Server class
#
#   Functions:      new
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

package RPC::XML::ServerMethod;

use 5.005;
use strict;
use vars qw($VERSION);
use subs qw();

require File::Spec;

$VERSION = do { my @r=(q$Revision: 1.1 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

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
#   Globals:        None.
#
#   Environment:    None.
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
        while (@argz)
        {
            ($key, $val) = splice(@argz, 0, 2);
            if ($key eq 'signature')
            {
                # Since there may be more than one signature, we allow it to
                # repeat. Of course, that's also why we can't just take @argz
                # directly as a hash. *shrug*
                $data->{signature} = [] unless $data->{$signature};
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
}

###############################################################################
#
#   Sub Name:       check
#
#   Description:    Boolean test to tell if the calling object has sufficient
#                   data to be used as a server method for RPC::XML::Server or
#                   Apache::RPC::Server.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object to test
#
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        Success:    1, valid/complete
#                   Failure:    0, invalid/incomplete
#
###############################################################################
sub check
{
    my $self = shift;

    return ((ref($self->{code}) eq 'CODE') and $self->{name} and
            (ref($self->{signature}) && scalar(@{$self->{signature}})));
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
#                   @path     in      list      Search path, used only if the
#                                                 filename itself is not abs.
#
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        Success:    hashref of values
#                   Failure:    error string
#
###############################################################################
sub load_XPL_file
{
    my $self = shift;
    my $file = shift;
    my @path = @_;

    require XML::Parser;

    my ($signature, $code, $codetext, $return, $accum, $P, %attr);
    local *F;

    unless (File::Spec->file_name_is_absolute($file))
    {
        my $path;
        for (@path)
        {
            $path = File::Spec->catfile($_, $file);
            if (-e $path) { $file = $path; last; }
        }
    }

    $return = {};
    # So these don't end up undef, since they're optional elements
    $return->{hidden} = 0; $return->{version} = ''; $return->{help} = '';
    $return->{signature} = [];
    open(F, "< $file");
    return "Error opening $file for reading: $!" if ($?);
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
                                   push(@{$return->{signature}},
                                        [ split(/ /, $accum) ]);
                               }
                               elsif ($elem eq 'code')
                               {
                                   $return->{$elem} = $accum
                                       unless ($attr{language} and
                                               $attr{language} ne 'perl');
                               }
                               else
                               {
                                   $return->{$elem} = $accum;
                               }

                               %attr = ();
                               $accum = '';
                           }});
    return "Error creating XML::Parser object" unless $P;
    # Trap any errors
    eval { $P->parse(*F) };
    return "Error parsing $file: $@" if $@;

    # Try to normalize $codetext before passing it to eval
    ($codetext = $return->{code}) =~
        s/sub[\s\n]+[\w:]+[\s\n]+\{/\$code = sub \{/;
    eval "$codetext";
    return "Error creating anonymous sub: $@" if $@;

    $return->{code} = $code;
    # The XML::Parser approach above gave us an empty "methoddef" key
    delete $return->{methoddef};
    # Add the file's mtime for when we check for stat-based reloading
    $return->{mtime} = (stat $file)[9];
    $return->{file} = $file;

    $return;
}
