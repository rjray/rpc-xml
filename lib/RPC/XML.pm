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
#   $Id: XML.pm,v 1.13 2002/05/22 09:43:49 rjray Exp $
#
#   Description:    This module provides the core XML <-> RPC conversion and
#                   structural management.
#
#   Functions:      This module contains many, many subclasses. Better to
#                   examine them individually.
#
#   Libraries:      RPC::XML::base64 uses MIME::Base64
#
#   Global Consts:  $VERSION
#
###############################################################################

package RPC::XML;

use 5.005;
use strict;
use vars qw(@EXPORT @EXPORT_OK %EXPORT_TAGS @ISA $VERSION $ERROR);
use subs qw(time2iso8601 smart_encode);

require Exporter;

@ISA = qw(Exporter);
@EXPORT_OK = qw(time2iso8601 smart_encode
                RPC_BOOLEAN RPC_INT RPC_DOUBLE RPC_NIL RPC_DATETIME_ISO8601
                RPC_DATETIME_INT RPC_BASE64 RPC_REFERENCE RPC_STRING);
%EXPORT_TAGS = (types => [ qw(RPC_BOOLEAN RPC_INT RPC_DOUBLE RPC_STRING
                              RPC_DATETIME_ISO8601 RPC_BASE64) ],
                all   => [ @EXPORT_OK ]);

$VERSION = do { my @r=(q$Revision: 1.13 $=~/\d+/g); sprintf "%d."."%02d"x$#r,@r };

# Global error string
$ERROR = '';

1;

# All of the RPC_* functions are convenience-encoders
sub RPC_STRING           ( $ ) { RPC::XML::string->new($_[0]) }
sub RPC_BOOLEAN          ( $ ) { RPC::XML::boolean->new($_[0]) }
sub RPC_INT              ( $ ) { RPC::XML::int->new($_[0]) }
sub RPC_DOUBLE           ( $ ) { RPC::XML::double->new($_[0]) }
sub RPC_DATETIME_ISO8601 ( $ ) { RPC::XML::datetime_iso8601->new($_[0]) }
sub RPC_BASE64           ( $ ) { RPC::XML::base64->new($_[0]) }

# This is a dead-simple ISO8601-from-UNIX-time stringifier. Always expresses
# time in UTC.
sub time2iso8601
{
    my $time = shift;
    my $zone = shift || '';

    my @time = gmtime($time);
    $time = sprintf("%4d%02d%02dT%02d:%02d:%02dZ",
                    $time[5] + 1900, $time[4] + 1, @time[3, 2, 1, 0]);
    if ($zone)
    {
        my $char = $zone > 0 ? '+' : '-';
        chop $time; # Lose the Z if we're specifying a zone
        $time .= $char . sprintf('%02d:00', abs($zone));
    }

    $time;
}

# This is a (futile?) attempt to provide a "smart" encoding method that will
# take a Perl scalar and promote it to the appropriate RPC::XML::_type_.
sub smart_encode
{
    my @values = @_;

    my $type;

    @values = map
    {
        if ($type = ref($_))
        {
            # Skip any that have already been encoded
            if (UNIVERSAL::isa($_, 'RPC::XML::datatype'))
            {
                $type = $_;
            }
            elsif ($type eq 'HASH')
            {
                $type = RPC::XML::struct->new($_);
            }
            elsif ($type eq 'ARRAY')
            {
                $type = RPC::XML::array->new($_);
            }
            else
            {
                # ??? Don't know what else to do
                next;
            }
        }
        # You have to check ints first, because they match the next pattern too
        elsif (/^[-+]?\d+$/)
        {
            $type = RPC::XML::int->new($_);
        }
        # Pattern taken from perldata(1)
        elsif (/^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/)
        {
            $type = RPC::XML::double->new($_);
        }
        else
        {
            $type = RPC::XML::string->new($_);
        }

        $type;
    } @values;

    return (wantarray ? @values : $values[0]);
}

# This is a (mostly) empty class used as a common superclass for simple and
# complex types, so that their derivatives may be universally type-checked.
package RPC::XML::datatype;
use vars qw(@ISA);
@ISA = ();

sub type { my $class = ref($_[0]) || $_[0]; $class =~ s/.*://; $class }
sub is_fault { 0 }

###############################################################################
#
#   Package:        RPC::XML::simple_type
#
#   Description:    A base class for the simpler type-classes to inherit from,
#                   for default constructor, stringification, etc.
#
###############################################################################
package RPC::XML::simple_type;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::datatype);

# new - a generic constructor that presumes the value being stored is scalar
sub new
{
    my $class = shift;
    my $value = shift;

    $RPC::XML::ERROR = '';
    $class = ref($class) || $class;
    bless \$value, $class;
}

# value - a generic accessor
sub value
{
    my $self = shift;

    $$self;
}

# as_string - return the value as an XML snippet
sub as_string
{
    my $self = shift;

    my $class;
    return unless ($class = ref($self));
    $class =~ s/^.*\://;
    $class =~ s/_/./g;
    substr($class, 0, 8) = 'dateTime' if (substr($class, 0, 8) eq 'datetime');

    "<$class>$$self</$class>";
}

###############################################################################
#
#   Package:        RPC::XML::int
#
#   Description:    Data-type class for integers
#
###############################################################################
package RPC::XML::int;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::simple_type);

###############################################################################
#
#   Package:        RPC::XML::i4
#
#   Description:    Data-type class for i4. Forces data into an int object.
#
###############################################################################
package RPC::XML::i4;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::simple_type);

###############################################################################
#
#   Package:        RPC::XML::double
#
#   Description:    The "double" type-class
#
###############################################################################
package RPC::XML::double;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::simple_type);

###############################################################################
#
#   Package:        RPC::XML::string
#
#   Description:    The "string" type-class
#
###############################################################################
package RPC::XML::string;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::simple_type);

# as_string - return the value as an XML snippet
sub as_string
{
    my $self = shift;

    my ($class, $value);

    return unless ($class = $self->type);

    ($value = $$self) =~ s/&/&amp;/g;
    $value            =~ s/</&lt;/g;
    $value            =~ s/>/&gt;/g;

    "<$class>$value</$class>";
}

###############################################################################
#
#   Package:        RPC::XML::boolean
#
#   Description:    The type-class for boolean data. Handles some "extra" cases
#
###############################################################################
package RPC::XML::boolean;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::simple_type);

# This constructor allows any of true, false, yes or no to be specified
sub new
{
    my $class = shift;
    my $value = shift || 0;

    $RPC::XML::ERROR = '';
    if ($value =~ /true|yes|1/i)
    {
        $value = 1;
    }
    elsif ($value =~ /false|no|0/i)
    {
        $value = 0;
    }
    else
    {
        $class = ref($class) || $class;
        $RPC::XML::ERROR = "${class}::new: Value must be one of yes, no, " .
            'true, false, 1, 0 (case-insensitive)';
        return undef;
    }

    bless \$value, $class;
}

###############################################################################
#
#   Package:        RPC::XML::datetime_iso8601
#
#   Description:    This is the class to manage ISO8601-style date/time values
#
###############################################################################
package RPC::XML::datetime_iso8601;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::simple_type);

sub type { 'dateTime.iso8601' };

###############################################################################
#
#   Package:        RPC::XML::array
#
#   Description:    This class encapsulates the array data type. Each element
#                   within the array should be one of the datatype classes.
#
###############################################################################
package RPC::XML::array;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::datatype);

# The constructor for this class mainly needs to sanity-check the value data
sub new
{
    my $class = shift;
    my @args = (ref($_[0]) eq 'ARRAY') ? @{$_[0]} : @_;

    # First ensure that each argument passed in is itself one of the data-type
    # class instances.
    for (@args)
    {
        $_ = RPC::XML::smart_encode($_)
            unless (UNIVERSAL::isa($_, 'RPC::XML::datatype'));
    }

    bless \@args, $class;
}

# This became more complex once it was shown that there may be a need to fetch
# the value while preserving the underlying objects.
sub value
{
    my $self = shift;
    my $no_recurse = shift || 0;
    my $ret;

    if ($no_recurse)
    {
        $ret = [ @$self ];
    }
    else
    {
        $ret = [ map { $_->value } @$self ];
    }

    $ret;
}

sub as_string
{
    my $self = shift;

    join('',
         '<array><data>',
         (map { ('<value>', $_->as_string(), '</value>') } (@$self)),
         '</data></array>');
}

###############################################################################
#
#   Package:        RPC::XML::struct
#
#   Description:    This is the "struct" data class. The struct is like Perl's
#                   hash, with the constraint that all values are instances
#                   of the datatype classes.
#
###############################################################################
package RPC::XML::struct;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::datatype);

# The constructor for this class mainly needs to sanity-check the value data
sub new
{
    my $class = shift;
    my %args = (ref($_[0]) eq 'HASH') ? %{$_[0]} : @_;

    # First ensure that each argument passed in is itself one of the data-type
    # class instances.
    for (keys %args)
    {
        $args{$_} = RPC::XML::smart_encode($args{$_})
            unless (UNIVERSAL::isa($args{$_}, 'RPC::XML::datatype'));
    }

    bless \%args, $class;
}

# This became more complex once it was shown that there may be a need to fetch
# the value while preserving the underlying objects.
sub value
{
    my $self = shift;
    my $no_recurse = shift || 0;
    my %value;

    if ($no_recurse)
    {
        %value = map { $_, $self->{$_} } (keys %$self);
    }
    else
    {
        %value = map { $_, $self->{$_}->value } (keys %$self);
    }

    \%value;
}

sub as_string
{
    my $self = shift;

    join('',
         '<struct>',
         (map {
             ("<member><name>$_</name><value>",
              $self->{$_}->as_string,
              '</value></member>')
         } (keys %$self)),
         '</struct>');
}

###############################################################################
#
#   Package:        RPC::XML::base64
#
#   Description:    This is the base64-encoding type. Plain data is passed in,
#                   plain data is returned. Plain is always returned. All the
#                   encoding/decoding is done behind the scenes.
#
###############################################################################
package RPC::XML::base64;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::simple_type);

use MIME::Base64;

sub new
{
    my ($class, $value, $encoded) = @_;

    $RPC::XML::ERROR = '';
    $value = $$value if (ref $value);
    unless (defined $value and length $value)
    {
        $class = ref($class) || $class;
        $RPC::XML::ERROR = "${class}::new: Must be called with non-null data";
        return undef;
    }
    if ($encoded)
    {
        $value = MIME::Base64::decode_base64 $value;
    }

    bless \$value, $class;
}

# The value needs to be encoded before being output
sub as_string
{
    my $self = shift;

    '<base64>' . MIME::Base64::encode_base64($$self) . '</base64>';
}

###############################################################################
#
#   Package:        RPC::XML::fault
#
#   Description:    This is the class that encapsulates the data for a RPC
#                   fault-response. Like the others, it takes the relevant
#                   information and maintains it internally. This is put
#                   at the end of the datum types, though it isn't really a
#                   data type in the sense that it cannot be passed in to a
#                   request. But it is separated so as to better generalize
#                   responses.
#
###############################################################################
package RPC::XML::fault;

use strict;
use vars qw(@ISA);

@ISA = qw(RPC::XML::struct);

# For our new(), we only need to ensure that we have the two required members
sub new
{
    my $class = shift;
    my @args = @_;

    my ($self, %args);

    $RPC::XML::ERROR = '';
    if (ref($args[0]) and UNIVERSAL::isa($args[0], 'RPC::XML::struct'))
    {
        # Take the keys and values from the struct object as our own
        %args = %{$args[0]->value('shallow')};
    }
    elsif (@args == 2)
    {
        # This is a special convenience-case to make simple new() calls clearer
        %args = (faultCode   => RPC::XML::int->new($args[0]),
                 faultString => RPC::XML::string->new($args[1]));
    }
    else
    {
        %args = @args;
    }

    unless ($args{faultCode} and $args{faultString})
    {
        $class = ref($class) || $class;
        $RPC::XML::ERROR = "${class}::new: Missing required struct fields";
        return undef;
    }
    if (scalar(keys %args) > 2)
    {
        $class = ref($class) || $class;
        $RPC::XML::ERROR = "${class}::new: Extra struct fields not allowed";
        return undef;
    }

    $self = $class->SUPER::new(%args);
}

# This only differs from the display of a struct in that it has some extra
# wrapped around it. Let the superclass as_string method do most of the work.
sub as_string
{
    my $self = shift;

    '<fault><value>' . $self->SUPER::as_string . '</value></fault>';
}

# Convenience methods:
sub code   { $_[0]->{faultCode}->value   }
sub string { $_[0]->{faultString}->value }

# This is the only one to override this method, for obvious reasons
sub is_fault { 1 }

###############################################################################
#
#   Package:        RPC::XML::request
#
#   Description:    This is the class that encapsulates the data for a RPC
#                   request. It takes the relevant information and maintains
#                   it internally until asked to stringify. Only then is the
#                   XML generated, encoding checked, etc. This allows for
#                   late-selection of <methodCall> or <methodCallSet> as a
#                   containing tag.
#
#                   This class really only needs a constructor and a method
#                   to stringify.
#
###############################################################################
package RPC::XML::request;

use strict;
use vars qw(@ISA);

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Creating a new request object, in this (reference) case,
#                   means checking the list of arguments for sanity and
#                   packaging it up for later use.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class/ref to bless into
#                   @argz     in      list      The exact disposition of the
#                                                 arguments is based on the
#                                                 type of the various elements
#
#   Returns:        Success:    object ref
#                   Failure:    undef, error in $RPC::XML::ERROR
#
###############################################################################
sub new
{
    my $class = shift;
    my @argz = @_;

    my ($self, $name);

    $class = ref($class) || $class;
    $RPC::XML::ERROR = '';

    unless (@argz)
    {
        $RPC::XML::ERROR = 'RPC::XML::request::new: At least a method name ' .
            'must be specified';
        return undef;
    }

    if (UNIVERSAL::isa($argz[0], 'RPC::XML::request'))
    {
        # Maybe this will be a clone operation
    }
    else
    {
        # This is the method name to be called
        $name = shift(@argz);
        # All the remaining args must be data.
        @argz = RPC::XML::smart_encode(@argz);
        $self = { args => [ @argz ], name => $name };
        bless $self, $class;
    }

    $self;
}

# Accessor methods
sub name       { shift->{name}       }
sub args       { shift->{args} || [] }

###############################################################################
#
#   Sub Name:       as_string
#
#   Description:    This is a fair bit more complex than the simple as_string
#                   methods for the datatypes. Express the invoking object as
#                   a well-formed XML document.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Invoking object
#                   $indent   in      scalar    Indention level for output
#
#   Returns:        Success:    text
#                   Failure:    undef
#
###############################################################################
sub as_string
{
    my $self   = shift;

    my $text;

    $RPC::XML::ERROR = '';

    $text = qq(<?xml version="1.0"?>\n);

    $text .= "<methodCall><methodName>$self->{name}</methodName><params>";
    for (@{$self->{args}})
    {
        $text .= '<param><value>' . $_->as_string . '</value></param>';
    }
    $text .= '</params></methodCall>';

    $text;
}

###############################################################################
#
#   Package:        RPC::XML::response
#
#   Description:    This is the class that encapsulates the data for a RPC
#                   response. As above, it takes the information and maintains
#                   it internally until asked to stringify. Only then is the
#                   XML generated, encoding checked, etc. This allows for
#                   late-selection of <methodResponse> or <methodResponseSet>
#                   as above.
#
###############################################################################
package RPC::XML::response;

use strict;
use vars qw(@ISA);

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Creating a new response object, in this (reference) case,
#                   means checking the outgoing parameter(s) for sanity.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class/ref to bless into
#                   @argz     in      list      The exact disposition of the
#                                                 arguments is based on the
#                                                 type of the various elements
#
#   Returns:        Success:    object ref
#                   Failure:    undef, error in $RPC::XML::ERROR
#
###############################################################################
sub new
{
    my $class = shift;
    my @argz = @_;

    my ($self, %extra, %attr);

    $class = ref($class) || $class;

    $RPC::XML::ERROR = '';
    if (! @argz)
    {
        $RPC::XML::ERROR = 'RPC::XML::response::new: One of a datatype, ' .
            'value or a fault object must be specified';
    }
    elsif (UNIVERSAL::isa($argz[0], 'RPC::XML::response'))
    {
        # This will eventually be a clone-operation. For now, just return in
        $self = $argz[0];
    }
    elsif (@argz > 1)
    {
        $RPC::XML::ERROR = 'RPC::XML::response::new: Responses may take ' .
            'only one argument';
    }
    else
    {
        $argz[0] = RPC::XML::smart_encode($argz[0]);

        $self = { value => $argz[0] };
        bless $self, $class;
    }

    $self;
}

# Accessor/status methods
sub value      { $_[0]->{value} }
sub is_fault   { $_[0]->{value}->is_fault }

###############################################################################
#
#   Sub Name:       as_string
#
#   Description:    This is a fair bit more complex than the simple as_string
#                   methods for the datatypes. Express the invoking object as
#                   a well-formed XML document.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Invoking object
#                   $indent   in      scalar    Indention level for output
#
#   Returns:        Success:    text
#                   Failure:    undef
#
###############################################################################
sub as_string
{
    my $self   = shift;

    my $text;

    $RPC::XML::ERROR = '';

    $text = qq(<?xml version="1.0"?>\n);

    $text .= '<methodResponse>';
    if ($self->{value}->isa('RPC::XML::fault'))
    {
        $text .= $self->{value}->as_string;
    }
    else
    {
        $text .= '<params><param><value>' . $self->{value}->as_string .
            '</value></param></params>';
    }
    $text .= '</methodResponse>';

    $text;
}


__END__

=head1 NAME

RPC::XML - A set of classes for core data, message and XML handling

=head1 SYNOPSIS

    use RPC::XML;

    $req = RPC::XML::request->new('fetch_prime_factors',
                                  RPC::XML::int->new(985120528));
    ...
    $resp = RPC::XML::Parser->new()->parse(STREAM);
    if (ref($resp))
    {
        return $resp->value->value;
    }
    else
    {
        die $resp;
    }

=head1 DESCRIPTION

The B<RPC::XML> package is a reference implementation of the XML-RPC
standard. As a reference implementation, it is geared more towards clarity and
readability than efficiency.

The package provides a set of classes for creating values to pass to the
constructors for requests and responses. These are lightweight objects, most
of which are implemented as tied scalars so as to associate specific type
information with the value. Classes are also provided for requests, responses,
faults (errors) and a parser based on the L<XML::Parser> package from CPAN.

This module does not actually provide any transport implementation or
server basis. For these, see L<RPC::XML::Client> and L<RPC::XML::Server>,
respectively.

=head1 EXPORTABLE FUNCTIONS

At present, only two functions are available for import. They must be
explicitly imported as part of the C<use> statement, or with a direct call to
C<import>:

=over 4

=item time2iso8601($time)

Convert the integer time value in C<$time> to a ISO 8601 string in the UTC
time zone. This is a convenience function for occassions when the return value
needs to be of the B<dateTime.iso8601> type, but the value on hand is the
return from the C<time> built-in.

=item smart_encode(@args)

Converts the passed-in arguments to datatype objects. Any that are already
encoded as such are passed through unchanged. The routine is called recursively
on hash and array references. Note that this routine can only deduce a certain
degree of detail about the values passed. Boolean values will be wrongly
encoded as integers. Pretty much anything not specifically recognizable will
get encoded as a string object. Thus, for types such as C<fault>, the ISO
time value, base-64 data, etc., the program must still explicitly encode it.
However, this routine will hopefully simplify things a little bit for a
majority of the usage cases.

=back

=head1 CLASSES

The classes provided by this module are broken into two groups: I<datatype>
classes and I<message> classes.

=head2 Data Classes

The following data classes are provided by this library. Each of these provide
at least C<new>, C<value>, C<as_string> and C<is_fault> methods. Note that
these classes are designed to create throw-away objects. There is currently no
mechanism for changing the value stored within one of these object after the
constructor returns. It is assumed that a new object would be created,
instead.

The C<new> methods are constructors, C<value> returns the value stored within
the object (processed recursively for arrays and structs), and C<as_string>
stringifies the object as a chunk of XML. The C<is_fault> method always
returns a false value (0), except when the object itself is of type
B<RPC::XML::fault>. In that case, the return value is true (1).  indention
level which is applied as a base indention for output. Other arguments are
specified with the classes.

=over 4

=item RPC::XML::int

Creates an integer value. Constructor expects the integer value as an
argument.

=item RPC::XML::i4

This is like the C<int> class.

=item RPC::XML::double

Creates a floating-point value.

=item RPC::XML::string

Creates an arbitrary string. No special encoding is done to the string (aside
from XML document encoding, covered later) with the exception of the C<E<lt>>,
C<E<gt>> and C<&> characters, which are XML-escaped during object creation,
and then reverted when the C<value> method is called.

=item RPC::XML::boolean

Creates a boolean value. The value returned will always be either of B<1>
or B<0>, for true or false, respectively. When calling the constructor, the
program may specify any of: C<0>, C<no>, C<false>, C<1>, C<yes>, C<true>.

=item RPC::XML::datetime_iso8601

Creates an instance of the XML-RPC C<dateTime.iso8601> type. The specification
for ISO 8601 may be found elsewhere. No processing is done to the data.

=item RPC::XML::base64

Creates an object that encapsulates a chunk of data that will be treated as
base-64 for transport purposes. The value may be passed in as either a string
or as a scalar reference. Additionally, a second (optional) parameter may be
passed, that if true identifies the data as already base-64 encoded. If so,
the data is decoded before storage. The C<value> method returns decoded data,
and the C<as_string> method encodes it before stringification.

=item RPC::XML::array

Creates an array object. The constructor takes zero or more data-type
instances as arguments, which are inserted into the array in the order
specified. C<value> returns an array reference of native Perl types. If a
non-null value is passed as an argument to C<value()>, then the array
reference will contain the datatype objects (a shallow copy rather than a deep
one).

=item RPC::XML::struct

Creates a struct object, the analogy of a hash table in Perl. The keys are
ordinary strings, and the values must all be data-type objects. The C<value>
method returns a hash table reference, with native Perl types in the values.
Key order is not preserved. Key strings are not encoded for special XML
characters, so the use of such (C<E<lt>>, C<E<gt>>, etc.) is discouraged. If a
non-null value is passed as an argument to C<value()>, then the hash
reference will contain the datatype objects (a shallow copy rather than a deep
one).

=item RPC::XML::fault

A fault object is a special case of the struct object that checks to ensure
that there are two keys, C<faultCode> and C<faultString>.

As a matter of convenience, since the contents of a B<RPC::XML::fault>
structure are specifically defined, the constructor may be called with exactly
two arguments, the first of which will be taken as the code, and the second
as the string. They will be converted to RPC::XML types automatically and
stored by the pre-defined key names.

Also as a matter of convenience, the fault class provides the following
accessor methods for directly retrieving the integer code and error string
from a fault object:

=over 4

=item code

=item string

=back

Both names should be self-explanatory. The values returned are Perl values,
not B<RPC::XML> class instances.

=back

=head2 Message Classes

The message classes are used both for constructing messages for outgoing
communication as well as representing the parsed contents of a received
message. Both implement the following methods:

=over 4

=item new

This is the constructor method for the two message classes. The response class
may have only a single value (as a response is currently limited to a single
return value), and requests may have as many arguments as appropriate. In both
cases, the arguments are passed to the exported C<smart_encode> routine
described earlier.

=item as_string

Returns the message object expressed as an XML document. The document will be
lacking in linebreaks and indention, as it is not targeted for human reading.

=back

The two message-object classes are:

=over 4

=item RPC::XML::request

This creates a request object. A request object expects the first argument to
be the name of the remote routine being called, and all remaining arguments
are the arguments to that routine. Request objects have the following methods
(besides C<new> and C<as_string>):

=over 4

=item name

The name of the remote routine that the request will call.

=item args

Returns a list reference with the arguments that will be passed. No arguments
will result in a reference to an empty list.

=back

=item RPC::XML::response

The response object is much like the request object in most ways. They may
take only one argument, as that is all the specification allows for in a
response. Responses have the following methods (in addition to C<new> and
C<as_string>):

=over 4

=item value

The value the response is returning. It will be a RPC::XML data-type.

=item is_fault

A boolean test whether or not the response is signalling a fault. This is
the same as taking the C<value> method return value and testing it, but is
provided for clarity and simplicity.

=back

=back

=head1 DIAGNOSTICS

All constructors return C<undef> upon failure, with the error message available
in the package-global variable B<C<$RPC::XML::ERROR>>.

=head1 CAVEATS

This began as a reference implementation in which clarity of process and
readability of the code took precedence over general efficiency. It is now
being maintained as production code, but may still have parts that could be
written more efficiently.

=head1 CREDITS

The B<XML-RPC> standard is Copyright (c) 1998-2001, UserLand Software, Inc.
See <http://www.xmlrpc.com> for more information about the B<XML-RPC>
specification.

=head1 LICENSE

This module is licensed under the terms of the Artistic License that covers
Perl. See <http://language.perl.com/misc/Artistic.html> for the
license itself.

=head1 SEE ALSO

L<RPC::XML::Client>, L<RPC::XML::Server>, L<RPC::XML::Parser>, L<XML::Parser>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut
