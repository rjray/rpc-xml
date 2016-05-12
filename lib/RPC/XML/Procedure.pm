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
#   Description:    This class abstracts out all the procedure-related
#                   operations from the RPC::XML::Server class. It also
#                   provides the RPC::XML::Method and RPC::XML::Function
#                   namespaces.
#
#   Functions:      new
#                   name        \
#                   code         \
#                   signature     \ These are the accessor functions for the
#                   help          / data in the object, though it's visible.
#                   version      /
#                   hidden      /
#                   clone
#                   add_signature
#                   delete_signature
#                   make_sig_table
#                   match_signature
#                   reload
#                   load_xpl_file
#                   call
#
#   Libraries:      XML::Parser (used only on demand in load_xpl_file)
#                   File::Spec
#
#   Global Consts:  $VERSION
#
#   Environment:    None.
#
###############################################################################

# Perl::Critic:
#
# We use explicit @ISA in RPC::XML::Method and RPC::XML::Function because it
# is faster than doing 'use base' when we're already in the same file.

## no critic (ProhibitExplicitISA)

package RPC::XML::Procedure;

use 5.008008;
use strict;
use warnings;
use vars qw($VERSION %VALID_TYPES);

use File::Spec;
use Scalar::Util 'blessed';

use RPC::XML 'smart_encode';

# This module also provides RPC::XML::Method and RPC::XML::Function
## no critic (ProhibitMultiplePackages)

$VERSION = '1.31';
$VERSION = eval $VERSION;    ## no critic (ProhibitStringyEval)

# This should match the set of type-classes defined in RPC::XML.pm. Note that
# we use "dateTime.iso8601" instead of "datetime_iso8601", because that is how
# it has to be in the signature.
%VALID_TYPES = map { $_ => 1 }
    (qw(int i4 i8 double string boolean dateTime.iso8601 nil array struct
        base64));

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
    my ($class , @argz) = @_;

    my $new_proc;    # This will be a hashref that eventually gets blessed

    if (ref $class)
    {
        return __PACKAGE__ . '::new: Must be called as a static method';
    }

    # There are three things that @argz could be:
    if (ref $argz[0])
    {
        # 1. A hashref containing all the relevant keys

        # Start wtih the defaults for the optional keys
        $new_proc = {
            namespace => q{},
            version   => 0,
            hidden    => 0,
            help      => q{},
        };
        # Copy everything from the hash, don't try to use it directly
        for (keys %{$argz[0]}) { $new_proc->{$_} = $argz[0]->{$_} }
    }
    elsif (@argz == 1)
    {
        # 2. Exactly one non-ref element, a file to load

        # Loading code from an XPL file, it can actually be of a type other
        # than how this constructor was called. So what we are going to do is
        # this: If $class is RPC::XML::Procedure, act like a factory method
        # and return whatever the file claims to be. Otherwise, the file has
        # to match $class or it's an error.
        ($new_proc, my $pkg) = load_xpl_file($argz[0]);
        if (! ref $new_proc)
        {
            # load_xpl_path signalled an error
            return $new_proc;
        }
        if ($class ne 'RPC::XML::Procedure' && $pkg ne $class)
        {
            return "${class}::new: File loaded ($argz[0]) must match " .
                'this calling class';
        }

        $class = $pkg;
    }
    else
    {
        # 3. If there is more than one arg, it's a sort-of-hash. That is, the
        #    key 'signature' is allowed to repeat.
        my ($key, $val);
        $new_proc = {
            namespace => q{},
            version   => 0,
            hidden    => 0,
            help      => q{},
            signature => [],
        };
        while (@argz)
        {
            ($key, $val) = splice @argz, 0, 2;
            if ($key eq 'signature')
            {
                # Since there may be more than one signature, we allow it to
                # repeat. Of course, that's also why we can't just take @argz
                # directly as a hash. *shrug*
                push @{$new_proc->{signature}},
                    ref $val ? join q{ } => @{$val} : $val;
            }
            else
            {
                $new_proc->{$key} = $val;
            }
        }
    }

    # A sanity check on the content of the object before we bless it:
    if (! ($new_proc->{name} && $new_proc->{code}))
    {
        return "${class}::new: Missing required data (name or code)";
    }
    if (($class ne 'RPC::XML::Function') &&
        (! ((exists $new_proc->{signature}) &&
            (ref($new_proc->{signature}) eq 'ARRAY') &&
            scalar(@{$new_proc->{signature}}))))
    {
        return "${class}::new: Missing required data (signatures)";
    }
    bless $new_proc, $class;

    # This needs to happen post-bless in case of error (for error messages)
    return $new_proc->make_sig_table;
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
#   Globals:        %VALID_TYPES
#
#   Returns:        Success:    $self
#                   Failure:    error message
#
###############################################################################
sub make_sig_table
{
    my $self = shift;

    my ($return, $rest, @rest);
    my $me = ref($self) . '::make_sig_table';

    delete $self->{sig_table};
    for my $sig (@{$self->{signature}})
    {
        ($return, @rest) = split / /, $sig;
        if (! $return)
        {
            return "$me: Invalid signature, cannot be null";
        }
        if (! $VALID_TYPES{$return})
        {
            return "$me: Unknown return type '$return'";
        }
        # Not going to add List::MoreUtils to my dependencies list, so suppress
        # this critic flag:
        ## no critic (ProhibitBooleanGrep)
        if (grep { ! $VALID_TYPES{$_} } @rest)
        {
            return "$me: One or more invalid types in signature";
        }

        $rest = join q{ } => @rest;
        # If the key $rest already exists, then this is a collision
        if ($self->{sig_table}->{$rest})
        {
            return
                "$me: Cannot have two different return values for one set " .
                "of params ($return vs. $self->{sig_table}->{$rest})";
        }

        $self->{sig_table}->{$rest} = $return;
    }

    return $self;
}

# These are basic accessor/setting functions for the various attributes

sub name { return shift->{name}; }    # "name" cannot be changed at this level
sub namespace { return shift->{namespace} || q{}; }    # Nor can "namespace"

sub help
{
    my ($self, $value) = @_;

    if ($value)
    {
        $self->{help} = $value;
    }

    return $self->{help};
}

sub version
{
    my ($self, $value) = @_;

    if ($value)
    {
        $self->{version} = $value;
    }

    return $self->{version};
}

sub hidden
{
    my ($self, $value) = @_;

    if ($value)
    {
        $self->{hidden} = $value;
    }

    return $self->{hidden};
}

sub code
{
    my ($self, $value) = @_;

    if ($value and ref $value eq 'CODE')
    {
        $self->{code} = $value;
    }

    return $self->{code};
}

sub signature
{
    my ($self, $sig) = @_;

    if ($sig)
    {
        if (ref $sig eq 'ARRAY')
        {
            my $old = $self->{signature};
            $self->{signature} = $sig;
            my $is_good = $self->make_sig_table;
            if (! ref $is_good)
            {
                # If it failed to re-init the table, restore the old list (and
                # old table). We don't have to check this return, since it had
                # worked before.
                $self->{signature} = $old;
                $self->make_sig_table;

                # Return an error message, since this failed:
                return ref($self) . "::signature: $is_good";
            }
        }
        else
        {
            # Anything not an array ref isn't useful
            return ref($self) . "::signature: Bad value '$sig'";
        }
    }

    # Return a copy of the array, not the original
    return [ @{$self->{signature}} ];
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
    for (keys %{$self})
    {
        next if $_ eq 'signature';
        $new_self->{$_} = $self->{$_};
    }
    if (! $self->isa('RPC::XML::Function'))
    {
        $new_self->{signature} = [ @{$self->{signature}} ];
    }

    return bless $new_self, ref $self;
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
    my ($self, @args) = @_;

    my (%sigs, $is_good, $old);

    # Preserve the original in case adding the new one causes a problem
    $old = $self->{signature};
    %sigs = map { $_ => 1 } @{$self->{signature}};
    for my $one_sig (@args)
    {
        my $sig_key = (ref $one_sig) ? join q{ } => @{$one_sig} : $one_sig;
        $sigs{$sig_key} = 1;
    }
    $self->{signature} = [ keys %sigs ];
    $is_good = $self->make_sig_table;
    if (! ref $is_good)
    {
        # Because this failed, we have to restore the old table and return
        # an error
        $self->{signature} = $old;
        $self->make_sig_table;
        return ref($self) . '::add_signature: Error re-hashing table: ' .
            $is_good;
    }

    return $self;
}

sub delete_signature
{
    my ($self, @args) = @_;

    my %sigs;

    my $old = $self->{signature};
    %sigs = map { $_ => 1 } @{$self->{signature}};
    for my $one_sig (@args)
    {
        my $sig_key = (ref $one_sig) ? join q{ } => @{$one_sig} : $one_sig;
        delete $sigs{$sig_key};
    }
    $self->{signature} = [ keys %sigs ];

    if (@{$self->{signature}} == 0)
    {
        # Don't have to re-run make_sig_table, because it's still valid for
        # this set:
        $self->{signature} = $old;
        return ref($self) . '::delete_signature: Cannot delete last signature';
    }

    # This can't fail, because deleting a signature will never cause an
    # ambiguity in the table like adding one could.
    return $self->make_sig_table;
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

    if (ref $sig)
    {
        $sig = join q{ } => @{$sig};
    }

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

    my $class = ref $self;
    my $me = "${class}::reload";

    if (! $self->{file})
    {
        return "$me: No file associated with method $self->{name}";
    }

    my ($newly_loaded) = load_xpl_file($self->{file});

    if (ref $newly_loaded)
    {
        # Update the information on this actual object
        for (keys %{$newly_loaded})
        {
            $self->{$_} = $newly_loaded->{$_};
        }
        # Re-calculate the signature table, in case that changed as well
        return $self->make_sig_table;
    }
    else
    {
        return "$me: Error loading $self->{file}: $newly_loaded";
    }
}

###############################################################################
#
#   Sub Name:       load_xpl_file
#
#   Description:    Load a XML-encoded method description (generally denoted
#                   by a *.xpl suffix) and return the relevant information.
#
#                   Note that this is not a method, it does not take $self as
#                   an argument.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $file     in      scalar    File to load
#
#   Returns:        Success:    hashref of values
#                   Failure:    error string
#
###############################################################################
sub load_xpl_file
{
    my $file = shift;

    require XML::Parser;

    my ($me, $new_proc, $signature, $code, $codetext, $accum, $P, $fh,
        $eval_ret, $class, %attr);

    $me = __PACKAGE__ . '::load_xpl_file';

    $new_proc = {};
    # So these don't end up undef, since they're optional elements
    $new_proc->{hidden}    = 0;
    $new_proc->{version}   = q{};
    $new_proc->{help}      = q{};
    $new_proc->{namespace} = __PACKAGE__;
    $P = XML::Parser->new(
        ErrorContext => 1,
        Handlers     => {
            Char => sub { $accum .= $_[1] },
            Start => sub { %attr = splice @_, 2 },
            End => sub {
                my $elem = $_[1];

                $accum =~ s/^\s+//;
                $accum =~ s/\s+$//;
                if ($elem eq 'signature')
                {
                    $new_proc->{signature} ||= [];
                    push @{$new_proc->{signature}}, $accum;
                }
                elsif ($elem eq 'hidden')
                {
                    $new_proc->{hidden} = 1;
                }
                elsif ($elem eq 'code')
                {
                    if (! ($attr{language} &&
                           $attr{language} ne 'perl'))
                    {
                        $new_proc->{$elem} = $accum;
                    }
                }
                elsif ('def' eq substr $elem, -3)
                {
                    $class = 'RPC::XML::' . ucfirst substr $elem, 0, -3;
                }
                else
                {
                    $new_proc->{$elem} = $accum;
                }

                %attr  = ();
                $accum = q{};
            }
        }
    );
    if (! $P)
    {
        return "$me: Error creating XML::Parser object";
    }
    open $fh, '<', $file or return "$me: Error opening $file for reading: $!";
    # Trap any errors
    $eval_ret = eval { $P->parse($fh); 1; };
    close $fh or return "$me: Error closing $file: $!";
    if (! $eval_ret)
    {
        return "$me: Error parsing $file: $@";
    }

    # Try to normalize $codetext before passing it to eval

    # Fudge a little and let them use '.' as a synonym for '::' in the
    # namespace hierarchy.
    $new_proc->{namespace} =~ s/[.]/::/g;

    # Next step is to munge away any actual subroutine name so that the eval
    # yields an anonymous sub. Also insert the namespace declaration.
    ($codetext = $new_proc->{code}) =~
        s/sub\s+(?:[\w:]+)?\s*[{]/sub \{ package $new_proc->{namespace}; /;
    $code = eval $codetext; ## no critic (ProhibitStringyEval)
    return "$me: Error creating anonymous sub: $@" if $@;

    $new_proc->{code} = $code;
    # Add the file's mtime for when we check for stat-based reloading, name
    # for reloading, and init the "called" counter to 0.
    $new_proc->{mtime}  = (stat $file)[9];
    $new_proc->{file}   = $file;
    $new_proc->{called} = 0;

    return ($new_proc, $class);
}

###############################################################################
#
#   Sub Name:       call
#
#   Description:    Encapsulates the invocation of the code block that the
#                   object is abstracting. Manages parameters, signature
#                   checking, etc.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $srv      in      ref       An object derived from the
#                                                 RPC::XML::Server class
#                   @params_in in     list      The params for the call itself
#
#   Globals:        None.
#
#   Environment:    None.
#
#   Returns:        Success:    value
#                   Failure:    RPC::XML::fault object
#
###############################################################################
sub call
{
    my ($self, $srv, @params_in) = @_;

    my (@paramtypes, @params, $signature, $resptype, $response, $name);

    $name = $self->name;
    # Create the param list.
    # The type for the response will be derived from the matching signature
    @paramtypes = map { $_->type } @params_in;
    @params     = map { $_->value } @params_in;
    $signature = join q{ } => @paramtypes;
    $resptype = $self->match_signature($signature);
    # Since there must be at least one signature with a return value (even
    # if the param list is empty), this tells us if the signature matches:
    if (! $resptype)
    {
        return $srv->server_fault(
            badsignature =>
            "method $name has no matching signature for the argument list: " .
            "[$signature]"
        );
    }
    elsif ($resptype eq 'dateTime.iso8601')
    {
        $resptype = 'datetime_iso8601';
    }

    # Set these in case the server object is part of the param list
    local $srv->{signature} =          ## no critic (ProhibitLocalVars)
        [ $resptype, @paramtypes ];
    local $srv->{method_name} = $name; ## no critic (ProhibitLocalVars)
    # For RPC::XML::Method (and derivatives), pass the server object
    if ($self->isa('RPC::XML::Method'))
    {
        unshift @params, $srv;
    }

    # Now take a deep breath and call the method with the arguments
    if (! eval { $response = $self->{code}->(@params); 1; })
    {
        # On failure, propagate user-generated RPC::XML::fault exceptions, or
        # transform Perl-level error/failure into such an object
        if (blessed $@ and $@->isa('RPC::XML::fault'))
        {
            return $@;
        }
        else
        {
            return $srv->server_fault(
                execerror => "Method $name returned error: $@"
            );
        }
    }

    # Increment the 'called' key on the proc UNLESS the proc is named
    # 'system.status' and has a boolean-true as the first param.
    if (! (($name eq 'system.status') &&
           @params_in &&
           ($paramtypes[0] eq 'boolean') &&
           $params[0]))
    {
        $self->{called}++;
    }
    # Create a suitable return value
    if (! ref $response)
    {
        if ($resptype eq 'scalar')
        {
            # Server code from the RPC::XML::Function class doesn't use
            # signatures, so if they didn't encode the returned value
            # themselves they're trusting smart_encode() to get it right.
            $response = smart_encode($response);
        }
        else
        {
            # We checked that this was valid earlier, so no need for further
            # tests here.
            $response = "RPC::XML::$resptype"->new($response);
        }
    }

    return $response;
}

###############################################################################
#
#   Description:    This is now an empty sub-class of RPC::XML::Procedure.
#                   It differs behaviorally from ::Procedure in that the
#                   RPC::XML::Server object is passed in the arguments list
#                   when the underlying code is invoked by call().
#
#   Functions:      None.
#
###############################################################################

package RPC::XML::Method;

use strict;
use warnings;
use vars qw(@ISA);

@ISA = qw(RPC::XML::Procedure);

###############################################################################
#
#   Description:    This is a type of Procedure that does no signature tests
#                   at either creation or invocation. Like RPC::XML::Procedure
#                   it does *not* get the RPC::XML::Server object when the
#                   underlying code is invoked by call().
#
#   Functions:      signature
#                   make_sig_table (called by some superclass methods)
#                   add_signature
#                   delete_signature
#                   match_signature
#
###############################################################################

package RPC::XML::Function;

use strict;
use warnings;
use vars qw(@ISA);
use subs qw(
    signature make_sig_table add_signature delete_signature match_signature
);

@ISA = qw(RPC::XML::Procedure);

# These are the bits that have to be different for RPC::XML::Function versus
# the other procedure types. They are simple-enough that they don't need
# dedicated comment-blocks for them.
sub signature        { return [ 'scalar' ]; }
sub make_sig_table   { return shift; }
sub add_signature    { return shift; }
sub delete_signature { return shift; }
sub match_signature  { return 'scalar'; }

1;

__END__

=head1 NAME

RPC::XML::Procedure - Object encapsulation of server-side RPC procedures

=head1 SYNOPSIS

    require RPC::XML::Procedure;

    ...
    $procedure = RPC::XML::Procedure->new({ name => 'system.identity',
                                            code => sub { ... },
                                            signature => [ 'string' ] });
    $method    = RPC::XML::Method->new('/path/to/status.xpl');
    $function  = RPC::XML::Function->new(name => 'add',
                                         code => sub { ... });

=head1 DESCRIPTION

The B<RPC::XML::Procedure> package is designed primarily for behind-the-scenes
use by the B<RPC::XML::Server> class and any subclasses of it. It is
documented here in case a project chooses to sub-class it for their purposes
(which would require setting the C<method_class> attribute when creating
server objects, see L<RPC::XML::Server|RPC::XML::Server>).

This package grew out of the increasing need to abstract the operations that
related to the methods a given server instance was providing. Previously,
methods were passed around simply as hash references. It was a small step then
to move them into a package and allow for operations directly on the objects
themselves. In the spirit of the original hashes, all the key data is kept in
clear, intuitive hash keys (rather than obfuscated as the other classes
do). Thus it is important to be clear on the interface here before
sub-classing this package.

=head1 CLASSES

This module provides three classes, representing the three types of procedures
that servers can use:

=over

=item Methods (B<RPC::XML::Method>)

Code that is considered a "method" by the server is called as though it were,
in fact, a method in that class. The first argument in the list is the server
object itself, with the arguments to the call making up the rest of the list.
The server checks the signature of the method against the arguments list
before the call is made. See below (L</"How Procedures Are Called">) for more
on the invocation of code as methods.

=item Procedures (B<RPC::XML::Procedure>)

Code that is considered a "procedure" by the server is called like a normal
(non-method) subroutine call. The server object is not injected into the
arguments list. The signature of the procedure is checked again the list of
arguments before the call is made, as with methods.

=item Functions (B<RPC::XML::Function>)

Lastly, code that is considered a "function" is the simplest of the three:
it does not have the server object injected into the arguments list, and no
check of signatures is done before the call is made. It is the responsibility
of the function to properly understand the arguments list, and to return a
value that the caller will understand.

=back

There is (currently) no version that is called like a method but ignores
signatures like a function.

=head1 SUBROUTINES/METHODS

The following methods are provided by this class:

=over 4

=item new(FILE|HASHREF|LIST)

Creates a new object of the class, and returns a reference to it. The
arguments to the constructor are variable in nature, depending on the type:

=over 8

=item FILE

If there is exactly on argument that is not a reference, it is assumed to be a
filename from which the method is to be loaded. This is presumed to be in the
B<XPL> format described below (see L</"XPL File Structure">). If the file
cannot be opened, or if once opened cannot be parsed, an error is raised.

=item HASHREF

If there is exactly one argument that is a reference, it is assumed to be a
hash with the relevant information on the same keys as the object itself
uses. This is primarily to support backwards-compatibility to code written
when methods were implemented simply as hash references.

=item LIST

If there is more than one argument in the list, then the list is assumed to be
a sort of "ersatz" hash construct, in that one of the keys (C<signature>) is
allowed to "stack" if it occurs multiple times. Otherwise, any keys that occur
multiple times overwrite the previous value:

=over 12

=item name

The name of the method, as it will be presented to clients

=item code

A reference to a subroutine, or an anonymous subroutine, that will receive
calls for the method

=item signature

Provides one calling-signature for the method, as either a space-separated
string of types or a list-reference

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
streamline the management of methods within the server classes, this must
persist. However, the other elements may be used in the creation of a new
object, which may then be added to the server, if the name absolutely must
change.

=item namespace

If the procedure object was created from a file, or if the instantiation
included namespace information, this accessor will return the namespace that
the underlying code executes in. Otherwise, it returns an empty string. This
cannot be altered (even if the B<code> method is used to replace the code
routine).

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

=item call(SERVER, PARAMLIST)

Execute the code that this object encapsulates, using the list of parameters
passed in PARAMLIST. The SERVER argument should be an object derived from the
B<RPC::XML::Server> class. For some types of procedure objects, this becomes
the first argument of the parameter list to simulate a method call as if it
were on the server object itself. The return value should be a data object
(possibly a B<RPC::XML::fault>), but may not always be pre-encoded. Errors
trapped in C<$@> are converted to fault objects. This method is generally used
in the C<dispatch> method of the server class, where the return value is
subsequently wrapped within a B<RPC::XML::response> object.

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

=item file

When the method was loaded from a file, this key contains the path to the file
used.

=item namespace

If the code is loaded from a file, this hash key will reflect what namespace
the code executes in. If the file specified a namespace, that is the value
you will get (any occurrence of C<.> in the specified namespace will have been
converted to C<::>). If no explicit namespace was provided, the namespace
of the class you called B<new> from will be used. See L</"Namespaces">.

=item mtime

When the method was loaded from a file, this key contains the
modification-time of the file, as a UNIX-style C<time> value. This is used to
check for changes to the file the code was originally read from.

=item called

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

=over 4

=item The XML-based file structure

The B<XPL Procedure Layout> dialect is a very simple application of XML to the
problem of expressing the method in such a way that it could be useful to
other packages than this one, or useful in other contexts than this one.

The lightweight DTD for the layout can be summarized as:

    <!ELEMENT  proceduredef  (name, namespace?, version?, hidden?,
                              signature+, help?, code)>
    <!ELEMENT  methoddef     (name, namespace?, version?, hidden?,
                              signature+, help?, code)>
    <!ELEMENT  functiondef   (name, namespace?, version?, hidden?,
                              signature+, help?, code)>
    <!ELEMENT  name       (#PCDATA)>
    <!ELEMENT  namespace  (#PCDATA)>
    <!ELEMENT  version    (#PCDATA)>
    <!ELEMENT  hidden     EMPTY>
    <!ELEMENT  signature  (#PCDATA)>
    <!ELEMENT  help       (#PCDATA)>
    <!ELEMENT  code       (#PCDATA)>
    <!ATTLIST  code       language (#PCDATA)>

The containing tag is always one of C<< <methoddef> >>, C<< <proceduredef> >>
or C<< <functiondef> >>. The tags that specify name, signatures and the code
itself must always be present. Some optional information may also be
supplied. The "help" text, or what an introspection API would expect to use to
document the method, is also marked as optional.  Having some degree of
documentation for all the methods a server provides is a good rule of thumb,
however.

The default methods that this package provides are turned into XPL files by the
B<make_method> tool (see L<make_method|make_method>). The final forms of these
may serve as examples of what the file should look like.

=item Information used only for book-keeping

Some of the information in the XPL file is only for book-keeping: the version
stamp of a method is never involved in the invocation. The server also keeps
track of the last-modified time of the file the method is read from, as well
as the full directory path to that file. The C<< <hidden /> >> tag is used
to identify those methods that should not be exposed to the outside world
through any sort of introspection/documentation API. They are still available
and callable, but the client must possess the interface information in order
to do so.

=item The information crucial to the method

The name, signatures and code must be present for obvious reasons. The
C<< <name> >> tag tells the server what external name this procedure is
known by. The C<< <signature> >> tag, which may appear more than once,
provides the definition of the interface to the function in terms of what
types and quantity of arguments it will accept, and for a given set of
arguments what the type of the returned value is. Lastly is the
C<< <code> >> tag, without which there is no procedure to remotely call.

=item Why the <code> tag allows multiple languages

Note that the C<< <code> >> tag is the only one with an attribute, in this
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

=back

=head2 The C<make_method> Utility

The utility script C<make_method> is provided as a part of this software
suite. It allows for the automatic creation of XPL files from either
command-line information or from template files. It has a wide variety of
features and options, and is out of the scope of this particular manual
page. The package F<Makefile.PL> features an example of engineering the
automatic generation of XPL files and their delivery as a part of the normal
Perl module build process. Using this tool is highly recommended over managing
XPL files directly. For the full details, see L<make_method|make_method>.

=head1 NAMESPACES

As default behavior, Perl code that is passed to C<eval> when a XPL file is
loaded gets put into the same namespace as the package used to load the XPL.
It is not an issue when you create your own B<RPC::XML::Procedure> (or
B<::Method> or B<::Function>) objects, as the code is already instantiated
into a given namespace.  This can be important if your code expects to call
routines in other loaded packages, utilize package-level globals, etc.

To give developers control over the namespace in XPL code, a new optional
tag C<< <namespace> >> was added in the 0.65 release. If this tag is present
in the XPL being read, it defines the namespace that the C<< <code> >> block
is evaluated in.

The value of the namespace tag is a string providing the namespace in either
the Perl-style of hierarchy parts separated by C<::>, or the style used by
Java, Perl6, etc., in which the parts are separated by C<.>. The latter
form is converted to Perl style for the evaluation of the code. If there is
no namespace declaration in a XPL file, the namespace of the class that
loads the XPL is used.

=head1 DIAGNOSTICS

Unless otherwise noted in the individual documentation sections, all methods
return the object reference on success, or a (non-reference) text string
containing the error message upon failure.

=head1 CAVEATS

Moving the method management to a separate class adds a good deal of overhead
to the general system. The trade-off in reduced complexity and added
maintainability should offset this.

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

L<RPC::XML::Server|RPC::XML::Server>, L<make_method|make_method>

=head1 AUTHOR

Randy J. Ray C<< <rjray@blackperl.com> >>

=cut
