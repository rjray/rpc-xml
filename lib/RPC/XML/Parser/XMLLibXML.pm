###############################################################################
#
# This file copyright (c) 2010-2011 by Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License 2.0 (http://www.opensource.org/licenses/artistic-license-2.0.php) or
# the GNU LGPL (http://www.opensource.org/licenses/lgpl-2.1.php).
#
###############################################################################
#
#   Description:    This is a container for the XML::LibXML::Parser class that
#                   implements the interface defined in RPC::XML::Parser.
#
#   Functions:      new
#                   parse
#                   parse_more
#                   parse_done
#                   dom_to_obj
#                   dom_request
#                   dom_response
#                   dom_value
#                   dom_base64
#
#   Libraries:      RPC::XML::Parser
#                   XML::LibXML
#
#   Global Consts:  $VERSION
#                   %VALIDTYPES
#
###############################################################################

package RPC::XML::Parser::XMLLibXML;

use 5.008008;
use strict;
use warnings;
use vars qw($VERSION %VALIDTYPES);
use subs qw(new parse parse_more parse_done
            dom_to_obj dom_request dom_response dom_value dom_base64);
use base 'RPC::XML::Parser';

use Scalar::Util 'reftype';
use XML::LibXML;

$VERSION = '1.21';
$VERSION = eval $VERSION; ## no critic (ProhibitStringyEval)

# This is to identify valid types that don't already have special handling
%VALIDTYPES = map { ($_, $_) } (qw(int i4 i8 double boolean));
$VALIDTYPES{'dateTime.iso8601'} = 'datetime_iso8601';

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Constructor-- save any important attributes and leave the
#                   heavy-lifting to XML::LibXML.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Class we're blessing in to
#                   %args     in      hash      Any additional arguments
#
#   Returns:        object reference
#
###############################################################################
sub new
{
    my ($class, %args) = @_;

    return bless \%args, $class;
}

###############################################################################
#
#   Sub Name:       parse
#
#   Description:    Parse the provided string or stream. If no string or stream
#                   is given, then initialize the push-parsing interface.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $stream   in      scalar    Either the string to parse or
#                                                 an open filehandle of sorts
#
#   Returns:        Success:    request or response object, or $self
#                   Failure:    error message
#
###############################################################################
sub parse
{
    my ($self, $stream) = @_;

    my $parser = XML::LibXML->new(
        no_network      => 1,
        expand_xinclude => 0,
        expand_entities => 1,
        load_ext_dtd    => 0,
        no_blanks       => 1
    );
    # I really don't need the full granularity of XML::LibXML::InputCallback
    # here, but the ext_ent_handler was not working with the version of
    # libxml2 on Apple's Snow Leopard.
    my $callbacks = XML::LibXML::InputCallback->new();
    $callbacks->register_callbacks([
        sub {
            my ($uri) = @_;

            return ($uri =~ m{^file:/}) ? 1 : 0;
        },
        sub {},
        undef,
        undef,
    ]);
    $parser->input_callbacks($callbacks);

    # RT58323: It's not enough to just test $stream, I have to check
    # defined-ness. A 0 or null-string should yield an error, not a push-parser
    # instance.
    if (! defined $stream)
    {
        # If no stream is given, initialize the DOM push-parser interface and
        # return the object ref
        $self->{parser} = $parser;
        $parser->init_push();

        return $self;
    }

    # Determine if the stream is a string or a filehandle, and use the apropos
    # method to parse it.
    my ($doc, $result);
    if (ref $stream)
    {
        if (reftype($stream) eq 'GLOB')
        {
            $result = eval {
                $doc = $parser->parse_fh($stream);
                1;
            };
            if (! $result)
            {
                # Certain cases cause $@ to be a XML::LibXML::Error object
                # instead of a string. So force it to stringify with qq().
                return qq($@);
            }
        }
        elsif (reftype($stream) eq 'SCALAR')
        {
            $result = eval {
                $doc = $parser->parse_string(${$stream});
                1;
            };
            if (! $result)
            {
                # Certain cases cause $@ to be a XML::LibXML::Error object
                # instead of a string. So force it to stringify with qq().
                return qq($@);
            }
        }
        else
        {
            return __PACKAGE__ . '::parse: Unusable reference type passed in';
        }
    }
    else
    {
        $result = eval {
            $doc = $parser->parse_string($stream);
            1;
        };
        if (! $result)
        {
            # Certain cases cause $@ to be a XML::LibXML::Error object
            # instead of a string. So force it to stringify with qq().
            return qq($@);
        }
    }

    return $self->dom_to_obj($doc);
}

###############################################################################
#
#   Sub Name:       parse_more
#
#   Description:    Feed another chunk of XML to the push-parser
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   @chunks   in      list      One or more chunks of XML
#
#   Returns:        Success:    $self
#                   Failure:    dies
#
###############################################################################
sub parse_more
{
    my ($self, @chunks) = @_;

    for (@chunks)
    {
        $self->{parser}->push($_);
    }

    return $self;
}

###############################################################################
#
#   Sub Name:       parse_done
#
#   Description:    Finish the push-parse process and convert the DOM structure
#                   to either RPC::XML::request or RPC::XML::response
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#
#   Returns:        Success:    One of RPC::XML::request or RPC::XML::response
#                   Failure:    dies
#
###############################################################################
sub parse_done
{
    my ($self) = @_;

    my $doc = $self->{parser}->finish_push();

    return $self->dom_to_obj($doc);
}

###############################################################################
#
#   Sub Name:       dom_to_obj
#
#   Description:    Take the XML::LibXML::Document object returned by the
#                   XML::LibXML::Parser::finish_push() call, and transform it
#                   into either a RPC::XML::request or RPC::XML::response
#                   object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $dom      in      ref       Object representing the parsed
#                                                 DOM of the original message.
#
#   Returns:        Success:    reference
#                   Failure:    error message
#
###############################################################################
sub dom_to_obj
{
    my ($self, $dom) = @_;

    # The support for SAX parsing in XML::LibXML is spotty and incomplete,
    # according to their docs. So I've chosen to use the push-style DOM parser
    # and then walk the resulting DOM to make sure that what I get is what I
    # needed.

    my ($element, $nodename, $retval);
    $element = $dom->documentElement();
    if (($nodename = $element->nodeName) =~ /^method(Call|Response)$/)
    {
        $retval = ($1 eq 'Call') ?
            $self->dom_request($element) : $self->dom_response($element);
    }
    else
    {
        return "Unknown tag: $nodename";
    }

    return $retval;
}

# Parse the part of the DOM rooted at $dom as a XML-RPC request
sub dom_request
{
    my ($self, $dom) = @_;

    my ($method_name, @args);
    my @nodes = $dom->childNodes;

    if (@nodes > 2)
    {
        return 'Extra content in "methodCall" block, starting with "' .
            $nodes[2]->nodeName . '" tag';
    }

    if ($nodes[0]->nodeName eq 'methodName')
    {
        $method_name = $nodes[0]->textContent;
        $method_name =~ s/^\s+//;
        $method_name =~ s/\s+$//;
        if ($method_name !~ m{^[\w.:/]+$})
        {
            return qq{methodName value "$method_name" not a valid name};
        }
    }
    else
    {
        return 'methodCall element missing "methodName" child-element';
    }

    if ($nodes[1])
    {
        if ($nodes[1]->nodeName eq 'params')
        {
            # All the heavy-lifting for parsing out params and/or building up
            # the compound structures is either done in dom_params() or futher
            # delegated by it:
            @args = $self->dom_params($nodes[1]);
            # Return if it was an error message
            if ($args[0] && ! ref $args[0])
            {
                return $args[0];
            }
        }
        else
        {
            return 'Unknown tag "' . $nodes[1]->nodeName . '" following ' .
                '"methodName" element';
        }
    }

    return RPC::XML::request->new($method_name, @args);
}

# Parse the part of the DOM rooted at $dom as a XML-RPC response
sub dom_response
{
    my ($self, $dom) = @_;

    my $param;
    my $me = __PACKAGE__ . '::dom_response';
    my @children = $dom->childNodes;

    if (1 != @children)
    {
        return "$me: Illegal content within methodResponse: " .
            'too many child elements';
    }
    my $node = $children[0];

    if ($node->nodeName eq 'params')
    {
        # This is like delegating to dom_params() in the parsing of a request,
        # but it is limited to a single value (which is why it has to be
        # tested here).
        @children = $node->childNodes;

        if (1 != @children)
        {
            return
                "$me: Illegal content within params: too many child elements";
        }
        elsif ($children[0]->nodeName ne 'param')
        {
            return qq($me: Invalid content within params: Unknown tag ") .
                $children[0]->nodeName . '", expected "param"';
        }

        # We know that $children[0] is the sole <param> tag. Look at its
        # content to see that we have exactly one <value> tag.
        @children = $children[0]->childNodes;

        if (1 != @children)
        {
            return
                "$me: Illegal content within param: too many child elements";
        }
        elsif ($children[0]->nodeName ne 'value')
        {
            return qq($me: Invalid content within params: Unknown tag ") .
                $children[0]->nodeName . '", expected "value"';
        }

        $param = $self->dom_value($children[0]);
        if (! ref $param)
        {
            # Return if it was an error message
            return $param;
        }
    }
    elsif ($node->nodeName eq 'fault')
    {
        # Make sure that we have a single <value></value> container
        my @sub_children = $node->childNodes;

        if (1 != @sub_children)
        {
            return
                "$me: Illegal content within fault: too many child elements";
        }
        elsif ($sub_children[0]->nodeName ne 'value')
        {
            return qq($me: Invalid content within fault tag: Unknown tag ") .
                $sub_children[0]->nodeName . '", expected "value"';
        }

        # Use the dom_value() routine that is generally called by dom_params()
        # to get the underlying struct out, then pass that to the constructor
        # of RPC::XML::fault:
        my $value = $self->dom_value($sub_children[0]);
        if (! ref $value)
        {
            # Return if it was an error message
            return $value;
        }
        if (! ref($param = RPC::XML::fault->new($value)))
        {
            # If it isn't a ref, then there was an error in creating the
            # fault object from $value
            return $RPC::XML::ERROR;
        }
    }
    else
    {
        return qq($me: Illegal tag ") . $node->nodeName .
            '" in "methodResponse" body';
    }

    return RPC::XML::response->new($param);
}

# Parse the <params> block, returning a list of the parsed <value> elements
sub dom_params
{
    my ($self, $node) = @_;
    my @values = ();
    my $me = __PACKAGE__ . '::dom_params';

    # The <params> block should contain zero or more <param> blocks, each of
    # which contains a single <value> block.
    for my $child ($node->childNodes)
    {
        if ((my $tag = $child->nodeName) ne 'param')
        {
            return "$me: Unknown tag in params: $tag (expected 'param')";
        }
        # There should be exactly one child, named 'value'
        my @children = $child->childNodes;

        if (1 != @children)
        {
            return "$me: Too many child-nodes for param tag";
        }
        if ((my $tag = $children[0]->nodeName) ne 'value')
        {
            return "$me: Unknown tag in param: $tag (expected 'value')";
        }

        push @values, $self->dom_value($children[0]);
    }

    return @values;
}

# Extract a single XML-RPC value from within a <value> tag and return the
# apropos RPC::XML::* instance.
sub dom_value ## no critic(ProhibitExcessComplexity)
{
    my ($self, $node) = @_;
    my ($nodename, $value);
    my $me = __PACKAGE__ . '::dom_value';

    # Make sure we have only one child-node
    my @children = $node->childNodes;

    if (1 != @children)
    {
        return "$me: Too many child-nodes for value tag";
    }

    # This is a place where I wish I could jump to 5.10 and use the nifty
    # given/when case-statement...
    $nodename = $children[0]->nodeName;
    if (($nodename eq '#text') || ($nodename eq 'string'))
    {
        $value = RPC::XML::string->new($children[0]->textContent);
    }
    elsif ($nodename eq 'base64')
    {
        # Defer the tricky bits of Base64 (spooling to file, etc.) to a
        # separate sub
        $value = $self->dom_base64($children[0]);
    }
    elsif ($nodename eq 'nil')
    {
        if ($children[0]->hasChildNodes())
        {
            return "$me: The nil tag must be empty";
        }

        # The string is a flag to force nil creation even if the global flag
        # isn't set. The undef is just to put the flag in the right place.
        $value = RPC::XML::nil->new(undef, 'nil is always allowed in parsing');
    }
    elsif (my $type = $VALIDTYPES{$nodename})
    {
        $value = $children[0]->textContent();
        $value =~ s/^\s+//;
        $value =~ s/\s+$//;
        # Some minimal data-integrity checking
        if ($type eq 'int' or $type eq 'i4' or $type eq 'i8')
        {
            if ($value !~ /^[-+]?\d+$/)
            {
                return "$me: Bad integer data read";
            }
        }
        elsif ($type eq 'double')
        {
            if ($value !~
                # Taken from perldata(1)
                /^[+-]?(?=\d|[.]\d)\d*(?:[.]\d*)?(?:[Ee](?:[+-]?\d+))?$/x)
            {
                return "$me: Bad floating-point data read";
            }
        }
        $type = 'RPC::XML::' . $type;
        # The 'encoded' argument is only relevant for base64, ignored by all
        # the others.
        $value = $type->new($value, 'encoded');
    }
    elsif ($nodename eq 'array')
    {
        @children = $children[0]->childNodes;

        if ((1 != @children) || ($children[0]->nodeName ne 'data'))
        {
            return "$me: array tag must have just one child element, 'data'";
        }
        @children = $children[0]->childNodes;

        # Make sure every child node is a <value> tag
        if (my @bad = grep { $_->nodeName() ne 'value' } @children)
        {
            return qq($me: Bad tag within array: got ") . $bad[0]->nodeName .
                '", expected "value"';
        }

        # Take the easy way out and use recursion to fill out an array ref
        # with the results of value-ifying each child node.
        $value = [];
        for (@children)
        {
            my $newval = $self->dom_value($_);
            if (ref $newval)
            {
                push @{$value}, $newval;
            }
            else
            {
                return $newval;
            }
        }

        # Convert to object form
        $value = RPC::XML::array->new(from => $value);
    }
    elsif ($nodename eq 'struct')
    {
        @children = $children[0]->childNodes;

        # Make sure every child node is a <member> tag
        if (my @bad = grep { $_->nodeName() ne 'member'} @children)
        {
            return qq($me: Bad tag within struct: got ") .
                $bad[0]->nodeName . '", expected "member"';
        }

        # This is a little more work than <array>, as each <member> must have
        # exactly one <name> and one <value> child-tag.
        $value = {};
        for my $member (@children)
        {
            my @mchildren = $member->childNodes;

            if (2 != @mchildren)
            {
                return "$me: Wrong number of nodes within struct/member, " .
                    'expecting 2 (name, value), got ' . scalar @mchildren;
            }
            if (! (($mchildren[0]->nodeName eq 'name') &&
                   ($mchildren[1]->nodeName eq 'value')))
            {
                return "$me: Bad content within struct/member: expected tags" .
                    ' "name" and "value", got tags "' .
                    $mchildren[0]->nodeName . q{" and "} .
                    $mchildren[1]->nodeName . q{"};
            }

            # As with arrays, let a recursive call to this routine handle the
            # creation of the value side. But check the value returned for any
            # errors.
            if (ref(my $mvalue = $self->dom_value($mchildren[1])))
            {
                $value->{$mchildren[0]->textContent} = $mvalue;
            }
            else
            {
                return $mvalue;
            }
        }

        # Convert what we have to an object form
        $value = RPC::XML::struct->new($value);
    }
    else
    {
        return qq($me: Unknown tag "$nodename" found within value tag);
    }

    return $value;
}

# The RPC::XML::base64 data-type has some special considerations, so handle it
# on its own merits rather than clutter up dom_value(), above.
sub dom_base64
{
    my ($self, $dom) = @_;

    my $value;
    my $me = __PACKAGE__ . '::dom_base64';

    if ($self->{base64_to_fh})
    {
        require File::Spec;
        require File::Temp;
        my $fh;
        my $tmpdir = File::Spec->tmpdir;

        if ($self->{base64_temp_dir})
        {
            $tmpdir = $self->{base64_temp_dir};
        }
        # Whee! Turns out File::Temp->new() croaks on error, rather than just
        # returning undef and setting $! the way you'd expect a failed attempt
        # at opening a file to do...
        $fh = eval { File::Temp->new(UNLINK => 1, DIR => $tmpdir) };
        if  (! $fh)
        {
            return "$me: Error opening temp file for base64: $@";
        }
        print {$fh} $dom->textContent;

        $value = RPC::XML::base64->new($fh, 'encoded');
    }
    else
    {
        $value = RPC::XML::base64->new($dom->textContent, 'encoded');
    }

    return $value;
}

1;

__END__

=head1 NAME

RPC::XML::Parser::XMLLibXML - A container class for XML::LibXML

=head1 SYNOPSIS

    # This class should rarely (if ever) be used directly:

    use RPC::XML::ParserFactory 'XML::LibXML';
    ...
    $P = RPC::XML::ParserFactory->new();
    $P->parse($message);

=head1 DESCRIPTION

This class implements the interface defined in the B<RPC::XML::Parser>
factory-class (see L<RPC::XML::Parser|RPC::XML::Parser>) using the
B<XML::LibXML> module to handle the actual manipulation of XML.

=head1 SUBROUTINES/METHODS

This module implements the public-facing methods as described in
L<RPC::XML::Parser|RPC::XML::Parser>:

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
well-formed, or not valid). If this method is called on a parser instance that
is not a push-parser, an exception is thrown.

=back

=head1 DIAGNOSTICS

All methods return some type of reference on success. The B<new> and B<parse>
methods return an error string on failure. The B<parse_more> and B<parse_done>
methods may throw exceptions, if the underlying B<XML::LibXML> parser
encounters a fatal error.

=head1 EXTERNAL ENTITIES

As of version 1.15 of this module (version 0.75 of the B<RPC::XML> suite),
external entities whose URI is a C<file:/> scheme (local file) are explicitly
ignored. This is for security purposes.

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

L<RPC::XML|RPC::XML>, L<RPC::XML::Parser|RPC::XML::Parser>,
L<XML::LibXML|XML::LibXML>

=head1 AUTHOR

Randy J. Ray <rjray@blackperl.com>

=cut
