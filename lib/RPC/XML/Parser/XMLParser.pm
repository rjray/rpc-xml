###############################################################################
#
# This file copyright (c) 2001-2010 Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License 2.0 (http://www.opensource.org/licenses/artistic-license-2.0.php) or
# the GNU LGPL (http://www.opensource.org/licenses/lgpl-2.1.php).
#
###############################################################################
#
#   Description:    This is the RPC::XML::Parser::XMLParser class, a container
#                   for the XML::Parser class.
#
#   Functions:      new
#                   parse
#                   message_init
#                   message_end
#                   tag_start
#                   error
#                   stack_error
#                   tag_end
#                   char_data
#                   extern_ent
#                   final
#
#   Libraries:      RPC::XML
#                   XML::Parser
#
#   Global Consts:  Uses $RPC::XML::ERROR
#
#   Environment:    None.
#
###############################################################################

package RPC::XML::Parser::XMLParser;

use 5.006001;
use strict;
use warnings;
use vars qw($VERSION);
use subs qw(error stack_error new message_init message_end tag_start tag_end
            final char_data parse);
use base 'RPC::XML::Parser';

# I'm not ready to add Readonly to my list of dependencies...
## no critic (ProhibitConstantPragma)

# These constants are only used by the internal stack machine
use constant PARSE_ERROR => 0;
use constant METHOD      => 1;
use constant METHODSET   => 2;
use constant RESPONSE    => 3;
use constant RESPONSESET => 4;
use constant STRUCT      => 5;
use constant ARRAY       => 6;
use constant DATATYPE    => 7;
use constant ATTR_SET    => 8;
use constant METHODNAME  => 9;
use constant VALUEMARKER => 10;
use constant PARAMSTART  => 11;
use constant PARAM       => 12;
use constant STRUCTMEM   => 13;
use constant STRUCTNAME  => 14;
use constant DATAOBJECT  => 15;
use constant PARAMLIST   => 16;
use constant NAMEVAL     => 17;
use constant MEMBERENT   => 18;
use constant METHODENT   => 19;
use constant RESPONSEENT => 20;
use constant FAULTENT    => 21;
use constant FAULTSTART  => 22;

# This is to identify valid types
use constant VALIDTYPES  => { map { ($_, 1) } qw(int i4 i8 string double
                                                 boolean dateTime.iso8601
                                                 base64 nil) };
# This maps XML tags to stack-machine tokens
use constant TAG2TOKEN   => { methodCall        => METHOD,
                              methodResponse    => RESPONSE,
                              methodName        => METHODNAME,
                              params            => PARAMSTART,
                              param             => PARAM,
                              value             => VALUEMARKER,
                              fault             => FAULTSTART,
                              array             => ARRAY,
                              struct            => STRUCT,
                              member            => STRUCTMEM,
                              name              => STRUCTNAME  };

# Members of the class
use constant M_STACK                => 0;
use constant M_CDATA                => 1;
use constant M_BASE64_TO_FH         => 2;
use constant M_BASE64_TEMP_DIR      => 3;
use constant M_SPOOLING_BASE64_DATA => 4;

use XML::Parser;

require RPC::XML;

$VERSION = '1.22';
$VERSION = eval $VERSION; ## no critic (ProhibitStringyEval)

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Constructor. Save any important attributes, leave the
#                   heavy lifting for the parse() routine and XML::Parser.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class we're initializing
#                   %attr     in      hash      Any extras the caller wants
#
#   Globals:        $RPC::XML::ERROR
#
#   Returns:        Success:    object ref
#                   Failure:    undef
#
###############################################################################
sub new
{
    my ($class, %attrs) = @_;

    my $self = [];

    while (my ($key, $val) = each %attrs)
    {
        if ($key eq 'base64_to_fh')
        {
            $self->[M_BASE64_TO_FH] = $val;
        }
        elsif ($key eq 'base64_temp_dir')
        {
            $self->[M_BASE64_TEMP_DIR] = $val;
        }
    }

    return bless $self, $class;
}

###############################################################################
#
#   Sub Name:       parse
#
#   Description:    Parse the requested string or stream. This behaves mostly
#                   like parse() in the XML::Parser namespace, but does some
#                   extra, as well.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $stream   in      scalar    Either the string to parse or
#                                                 an open filehandle of sorts
#
#   Returns:        Success:    ref to request or response object
#                   Failure:    error string
#
###############################################################################
sub parse
{
    my ($self, $stream) = @_;

    my $parser = XML::Parser->new(Namespaces => 0,
                                  ParseParamEnt => 0,
                                  ErrorContext => 1,
                                  Handlers =>
                                  {
                                   Init      => sub { message_init $self, @_ },
                                   Start     => sub { tag_start    $self, @_ },
                                   End       => sub { tag_end      $self, @_ },
                                   Char      => sub { char_data    $self, @_ },
                                   Final     => sub { final        $self, @_ },
                                   ExternEnt => sub { extern_ent   $self, @_ },
                                  });

    # If there is no stream given, then create an incremental parser handle
    # and return it.
    # RT58323: It's not enough to just test $stream, I have to check
    # defined-ness. A 0 or null-string should yield an error, not a push-parser
    # instance.
    if (! defined $stream)
    {
        return $parser->parse_start();
    }

    my $retval;
    if (! eval { $retval = $parser->parse($stream); 1; })
    {
        # I'm not sure we could get here and NOT have $@ set, but check it
        # just in case...
        if ($@)
        {
            return $@;
        }
    }

    return $retval;
}

# This is called when a new document is about to start parsing
sub message_init
{
    my ($robj, $self) = @_;

    $robj->[M_STACK] = [];
    return $self;
}

# This is called when the parsing process is complete. There is a second arg,
# $self, that is passed but not used. So it isn't declared for now.
sub final
{
    my ($robj) = @_;

    # Look at the top-most marker, it'll need to be one of the end cases
    my $marker = pop @{$robj->[M_STACK]};
    # There should be only on item on the stack after it
    my $retval = pop @{$robj->[M_STACK]};
    # If the top-most marker isn't the error marker, check the stack
    if ($marker != PARSE_ERROR and (@{$robj->[M_STACK]}))
    {
        $retval = 'RPC::XML Error: Extra data on parse stack at document end';
    }

    return $retval;
}

# This gets called each time an opening tag is parsed. In addition to the three
# args here, any attributes are passed in hash form as well. But the XML-RPC
# spec uses no attributes, so we aren't declaring them here as the list will
# (or should, at least) always be empty.
sub tag_start
{
    my ($robj, $self, $elem) = @_;

    $robj->[M_CDATA] = [];
    return if ($elem eq 'data');

    if (TAG2TOKEN->{$elem})
    {
        push @{$robj->[M_STACK]}, TAG2TOKEN->{$elem};
    }
    elsif (VALIDTYPES->{$elem})
    {
        # All datatypes are represented on the stack by this generic token
        push @{$robj->[M_STACK]}, DATATYPE;
        # If the tag is <base64> and we've been told to use filehandles, set
        # that up.
        if ($elem eq 'base64')
        {
            if (! $robj->[M_BASE64_TO_FH])
            {
                return;
            }

            require Symbol;
            require File::Spec;
            require File::Temp;
            my ($fh, $tmpdir) = (Symbol::gensym(), File::Spec->tmpdir);

            if ($robj->[M_BASE64_TEMP_DIR])
            {
                $tmpdir = $robj->[M_BASE64_TEMP_DIR];
            }
            if (! ($fh = File::Temp->new(UNLINK => 1, DIR => $tmpdir)))
            {
                push @{$robj->[M_STACK]},
                    "Error opening temp file for base64: $!", PARSE_ERROR;
                $self->finish;
            }
            $robj->[M_CDATA] = $fh;
            $robj->[M_SPOOLING_BASE64_DATA]= 1;
        }
    }
    else
    {
        push @{$robj->[M_STACK]},
            "Unknown tag encountered: $elem", PARSE_ERROR;
        $self->finish;
    }

    return;
}

# Very simple error-text generator, just to eliminate heavy reduncancy in the
# next sub:
sub error
{
    my ($robj, $self, $mesg, $elem) = @_;
    $elem ||= q{};

    my $fmt = $elem ?
        '%s at document line %d, column %d (byte %d, closing tag %s)' :
        '%s at document line %d, column %d (byte %d)';

    push @{$robj->[M_STACK]},
        sprintf($fmt, $mesg, $self->current_line, $self->current_column,
                $self->current_byte, $elem),
        PARSE_ERROR;
    $self->finish;

    return;
}

# A shorter-cut for stack integrity errors
sub stack_error
{
    my ($robj, $self, $elem) = @_;

    return error($robj, $self, 'Stack corruption detected', $elem);
}

# This is a hairy subroutine-- what to do at the end-tag. The actions range
# from simply new-ing a datatype all the way to building the final object.
sub tag_end ## no critic (ProhibitExcessComplexity)
{
    my ($robj, $self, $elem) = @_;

    my ($op, $obj, $class, $list, $name);

    return if ($elem eq 'data');
    # This should always be one of the stack machine ops defined above
    $op = pop @{$robj->[M_STACK]};

    my $cdata = q{};
    if ($robj->[M_SPOOLING_BASE64_DATA])
    {
        $cdata = $robj->[M_CDATA];
        seek $cdata, 0, 0;
    }
    elsif ($robj->[M_CDATA])
    {
        $cdata = join q{} => @{$robj->[M_CDATA]};
    }

    # Decide what to do from here
    if (VALIDTYPES->{$elem}) ## no critic (ProhibitCascadingIfElse)
    {
        # This is the closing tag of one of the data-types.
        $class = $elem;
        # Cheaper than the regex that was here, and more locale-portable
        if ($class eq 'dateTime.iso8601')
        {
            $class = 'datetime_iso8601';
        }
        # Some minimal data-integrity checking
        if ($class eq 'int' or $class eq 'i4' or $class eq 'i8')
        {
            if ($cdata !~ /^[-+]?\d+$/)
            {
                return error($robj, $self, 'Bad integer data read');
            }
        }
        elsif ($class eq 'double')
        {
            if ($cdata !~
                # Taken from perldata(1)
                /^([+-]?)(?=\d|\.\d)\d*(\.\d*)?([Ee]([+-]?\d+))?$/x)
            {
                return error($robj, $self, 'Bad floating-point data read');
            }
        }
        elsif ($class eq 'nil')
        {
            # We now allow parsing of <nil/> at all times.
            # By definition though, it must be, well... nil.
            if ($cdata !~ /^\s*$/)
            {
                return error($robj, $self, '<nil /> element must be empty');
            }
        }

        $class = "RPC::XML::$class";
        # The string at the end is only seen by the RPC::XML::base64 class
        $obj = $class->new($cdata, 'base64 is encoded, nil is allowed');
        if (! $obj)
        {
            return error($robj, $self, 'Error instantiating data object: ' .
                         $RPC::XML::ERROR);
        }
        push @{$robj->[M_STACK]}, $obj, DATAOBJECT;
        if ($robj->[M_SPOOLING_BASE64_DATA])
        {
            $robj->[M_SPOOLING_BASE64_DATA] = 0;
            $robj->[M_CDATA] = undef; # Doesn't close FH, $obj still holds it
        }
    }
    elsif ($elem eq 'value')
    {
        # For <value></value>, there should already be a dataobject, or else
        # the marker token in which case the CDATA is used as a string value.
        if ($op == DATAOBJECT)
        {
            ($op, $obj) = splice @{$robj->[M_STACK]}, -2;
            if ($op != VALUEMARKER)
            {
                return stack_error($robj, $self, $elem);
            }
        }
        elsif ($op == VALUEMARKER)
        {
            $obj = RPC::XML::string->new($cdata);
        }
        else
        {
            return error($robj, $self,
                         'No datatype found within <value> container');
        }

        push @{$robj->[M_STACK]}, $obj, DATAOBJECT;
    }
    elsif ($elem eq 'param')
    {
        # Almost like above, since this is really a NOP anyway
        if ($op != DATAOBJECT)
        {
            return error($robj, $self,
                         'No <value> found within <param> container');
        }
        ($op, $obj) = splice @{$robj->[M_STACK]}, -2;
        if ($op != PARAM)
        {
            return stack_error($robj, $self, $elem);
        }
        push @{$robj->[M_STACK]}, $obj, DATAOBJECT;
    }
    elsif ($elem eq 'params')
    {
        # At this point, there should be zero or more DATAOBJECT tokens on the
        # stack, each with a data object right below it.
        $list = [];
        if (! ($op == DATAOBJECT or $op == PARAMSTART))
        {
            return stack_error($robj, $self, $elem);
        }
        while ($op == DATAOBJECT)
        {
            unshift @{$list}, pop @{$robj->[M_STACK]};
            $op = pop @{$robj->[M_STACK]};
        }
        # Now that we see something ! DATAOBJECT, it needs to be PARAMSTART
        if ($op != PARAMSTART)
        {
            return stack_error($robj, $self, $elem);
        }
        push @{$robj->[M_STACK]}, $list, PARAMLIST;
    }
    elsif ($elem eq 'fault')
    {
        # If we're finishing up a fault definition, there needs to be a struct
        # on the stack.
        if ($op != DATAOBJECT)
        {
            return stack_error($robj, $self, $elem);
        }
        ($op, $obj) = splice @{$robj->[M_STACK]}, -2;
        if (! $obj->isa('RPC::XML::struct'))
        {
            return error($robj, $self,
                         'Only a <struct> value may be within a <fault>');
        }
        $obj = RPC::XML::fault->new($obj);
        if (! $obj)
        {
            return error($robj, $self, 'Unable to instantiate fault object: ' .
                         $RPC::XML::ERROR);
        }

        push @{$robj->[M_STACK]}, $obj, FAULTENT;
    }
    elsif ($elem eq 'member')
    {
        # We need to see a DATAOBJECT followed by a STRUCTNAME
        if ($op != DATAOBJECT)
        {
            return stack_error($robj, $self, $elem);
        }
        ($op, $obj) = splice @{$robj->[M_STACK]}, -2;
        if ($op != STRUCTNAME)
        {
            return stack_error($robj, $self, $elem);
        }
        # Get the name off the stack to clear the way for the STRUCTMEM marker
        # under it
        ($op, $name) = splice @{$robj->[M_STACK]}, -2;
        # Push the name back on, with the value and the new marker (STRUCTMEM)
        push @{$robj->[M_STACK]}, $name, $obj, STRUCTMEM;
    }
    elsif ($elem eq 'name')
    {
        # Fairly simple: just push the current content of CDATA on w/ a marker
        push @{$robj->[M_STACK]}, $cdata, STRUCTNAME;
    }
    elsif ($elem eq 'struct')
    {
        # Create the hash table in-place, then pass the ref to the constructor
        $list = {};
        # First off the stack needs to be STRUCTMEM or STRUCT
        if (! ($op == STRUCTMEM or $op == STRUCT))
        {
            return stack_error($robj, $self, $elem);
        }
        while ($op == STRUCTMEM)
        {
            # Next on stack (in list-order): name, value
            ($name, $obj) = splice @{$robj->[M_STACK]}, -2;
            $list->{$name} = $obj;
            $op = pop @{$robj->[M_STACK]};
        }
        # Now that we see something ! STRUCTMEM, it needs to be STRUCT
        if ($op != STRUCT)
        {
            return stack_error($robj, $self, $elem);
        }
        $obj = RPC::XML::struct->new($list);
        if (! $obj)
        {
            return error($robj, $self,
                         'Error creating a RPC::XML::struct object: ' .
                         $RPC::XML::ERROR);
        }

        push @{$robj->[M_STACK]}, $obj, DATAOBJECT;
    }
    elsif ($elem eq 'array')
    {
        # This is similar in most ways to struct creation, save for the lack
        # of naming for the elements.
        # Create the list in-place, then pass the ref to the constructor
        $list = [];
        # Only DATAOBJECT or ARRAY should be visible
        if (! ($op == DATAOBJECT or $op == ARRAY))
        {
            return stack_error($robj, $self, $elem);
        }
        while ($op == DATAOBJECT)
        {
            unshift @{$list}, pop @{$robj->[M_STACK]};
            $op = pop @{$robj->[M_STACK]};
        }
        # Now that we see something ! DATAOBJECT, it needs to be ARRAY
        if ($op != ARRAY)
        {
            return stack_error($robj, $self, $elem);
        }
        # Use the special-form of the constructor, for when a listref should
        # be dereferenced by the constructor (to avoid doing it here and
        # possibly creating a huge stack):
        $obj = RPC::XML::array->new(from => $list);
        if (! $obj)
        {
            return error($robj, $self,
                         'Error creating a RPC::XML::array object: ' .
                         $RPC::XML::ERROR);
        }

        push @{$robj->[M_STACK]}, $obj, DATAOBJECT;
    }
    elsif ($elem eq 'methodName')
    {
        if ($robj->[M_STACK]->[$#{$robj->[M_STACK]}] != METHOD)
        {
            return error(
                $robj, $self,
                "<$elem> tag must immediately follow a <methodCall> tag"
            );
        }
        push @{$robj->[M_STACK]}, $cdata, NAMEVAL;
    }
    elsif ($elem eq 'methodCall')
    {
        # A methodCall closing should have on the stack an optional PARAMLIST
        # marker, a NAMEVAL marker, then the METHOD token from the
        # opening tag. An ATTR_SET may follow the METHOD token.
        if ($op == PARAMLIST)
        {
            ($op, $list) = splice @{$robj->[M_STACK]}, -2;
        }
        else
        {
            $list = [];
        }
        if ($op == NAMEVAL)
        {
            ($op, $name) = splice @{$robj->[M_STACK]}, -2;
        }
        if (! $name)
        {
            return error(
                $robj, $self,
                'No methodName tag detected during methodCall parsing'
            );
        }
        if ($op != METHOD)
        {
            return stack_error($robj, $self, $elem);
        }
        # Create the request object and push it on the stack
        $obj = RPC::XML::request->new($name, @{$list});
        if (! $obj)
        {
            return error($robj, $self,
                         "Error creating request object: $RPC::XML::ERROR");
        }

        push @{$robj->[M_STACK]}, $obj, METHODENT;
    }
    elsif ($elem eq 'methodResponse')
    {
        # A methodResponse closing should have on the stack only the
        # DATAOBJECT marker, then the RESPONSE token from the opening tag.
        if ($op == PARAMLIST)
        {
            # To my knowledge, the XML-RPC spec limits the params list for
            # a response to exactly one object. Extract it from the listref
            # and put it back.
            $list = pop @{$robj->[M_STACK]};
            if (1 != @{$list})
            {
                return error($robj, $self,
                             "Params list for <$elem> tag invalid");
            }
            $obj = $list->[0];
            if (! (ref $obj and $obj->isa('RPC::XML::datatype')))
            {
                return error($robj, $self,
                             'Returned value on stack not a type reference');
            }
            push @{$robj->[M_STACK]}, $obj;
        }
        elsif (! ($op == DATAOBJECT or $op == FAULTENT))
        {
            return error($robj, $self,
                         "No parameter was declared for the <$elem> tag");
        }
        ($op, $list) = splice @{$robj->[M_STACK]}, -2;
        if ($op != RESPONSE)
        {
            return stack_error($robj, $self, $elem);
        }
        # Create the response object and push it on the stack
        $obj = RPC::XML::response->new($list);
        if (! $obj)
        {
            return error($robj, $self,
                         "Error creating response object: $RPC::XML::ERROR");
        }

        push @{$robj->[M_STACK]}, $obj, RESPONSEENT;
    }

    return;
}

# This just spools the character data until a closing tag makes use of it
sub char_data
{
     my ($robj, undef, $data) = @_;

     if ($robj->[M_SPOOLING_BASE64_DATA])
     {
         print {$robj->[M_CDATA]} $data;
     }
     else
     {
         push @{$robj->[M_CDATA]}, $data;
     }

     return;
}

# At some future point, this may be expanded to provide more entities than
# just the four basic XML ones.
sub extern_ent
{
    my $robj = shift;

    local $" = ', ';
    warn ref($robj) . '::extern_ent: Attempt to reference external entity ' .
        "(@_)\n";

    return q{};
}

# Exception-throwing stub in case this is called without first getting the
# XML::Parser::ExpatNB instance:
sub parse_more
{
    die __PACKAGE__ . '::parse_more: Must be called on a push-parser ' .
        "instance obtained from parse()\n";
}

# Exception-throwing stub in case this is called without first getting the
# XML::Parser::ExpatNB instance:
sub parse_done
{
    die __PACKAGE__ . '::parse_done: Must be called on a push-parser ' .
        "instance obtained from parse()\n";
}

1;

__END__

=head1 NAME

RPC::XML::Parser::XMLParser - A container class for XML::Parser

=head1 SYNOPSIS

    # This class should rarely (if ever) be used directly:

    use RPC::XML::ParserFactory 'XML::Parser';
    ...
    $P = RPC::XML::ParserFactory->new();
    $P->parse($message);

=head1 DESCRIPTION

This class implements the interface defined in the B<RPC::XML::Parser>
factory-class (see L<RPC::XML::Parser>) using the B<XML::Parser> module
to handle the actual manipulation of XML.

=head1 SUBROUTINES/METHODS

This module implements the public-facing methods as described in
L<RPC::XML::Parser>:

=over 4

=item new [ ARGS ]

The constructor only recognizes the two parameters specified in the base
class (for the B<RPC::XML::base64> file-spooling operations).

=item parse [ STRING | STREAM ]

The parse() method accepts either a string of XML, a filehandle of some sort,
or no argument at all. In the latter case, the return value is a parser
instance that acts as a push-parser (a non-blocking parser). For the first
two types of input, the return value is either a message object (one of
B<RPC::XML::request> or B<RPC::XML::response>) or an error.

=item parse_more STRING

(Only callable on a push-parser instance) Parses the chunk of XML, which does
not have to describe a complete document, and adds it to the current running
document. If this method is called on a parser instance that is not a
push-parser, an exception is thrown.

=item parse_done

(Only callable on a push-parser instance) Finishes the parsing process and
returns either a message object (one of B<RPC::XML::request> or
B<RPC::XML::response>) or an error (if the document was incomplete, not
wel-formed, or not valid). If this method is called on a parser instance that
is not a push-parser, an exception is thrown.

=back

=head1 DIAGNOSTICS

All methods return some type of reference on success, or an error string on
failure. Non-reference return values should always be interpreted as errors,
except in the case of C<simple_request>.

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

L<RPC::XML>, L<RPC::XML::Parser>, L<XML::Parser>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut
