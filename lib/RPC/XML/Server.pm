###############################################################################
#
# This file copyright (c) 2001-2014 Randy J. Ray, all rights reserved
#
# Copying and distribution are permitted under the terms of the Artistic
# License 2.0 (http://www.opensource.org/licenses/artistic-license-2.0.php) or
# the GNU LGPL (http://www.opensource.org/licenses/lgpl-2.1.php).
#
###############################################################################
#
#   Description:    This class implements an RPC::XML server, using the core
#                   XML::RPC transaction code. The server may be created with
#                   or without an HTTP::Daemon object instance to answer the
#                   requests.
#
#   Functions:      new
#                   version
#                   url
#                   product_tokens
#                   started
#                   path
#                   host
#                   port
#                   requests
#                   response
#                   compress
#                   compress_thresh
#                   compress_re
#                   message_file_thresh
#                   message_temp_dir
#                   xpl_path
#                   add_method
#                   method_from_file
#                   get_method
#                   server_loop
#                   post_configure_hook
#                   pre_loop_hook
#                   process_request
#                   dispatch
#                   call
#                   add_default_methods
#                   add_methods_in_dir
#                   delete_method
#                   list_methods
#                   share_methods
#                   copy_methods
#                   timeout
#                   server_fault
#
#   Libraries:      HTTP::Daemon (conditionally)
#                   HTTP::Response
#                   HTTP::Status
#                   URI
#                   Scalar::Util
#                   RPC::XML
#                   RPC::XML::ParserFactory
#                   RPC::XML::Procedure
#                   Compress::Raw::Zlib is used if available
#
#   Global Consts:  $VERSION
#                   $INSTALL_DIR
#                   %FAULT_TABLE
#
###############################################################################

package RPC::XML::Server;

use 5.008008;
use strict;
use warnings;
use vars qw($VERSION $INSTALL_DIR %FAULT_TABLE  @XPL_PATH %CLASS_MAP
            $IO_SOCKET_SSL_HACK_NEEDED $COMPRESSION_AVAILABLE);

use Carp qw(carp croak);
use File::Spec;
use File::Temp;
use IO::Handle;
use Module::Load;
use Scalar::Util 'blessed';

use HTTP::Status;
use HTTP::Response;
use URI;

use RPC::XML;
use RPC::XML::ParserFactory;
use RPC::XML::Procedure;

BEGIN
{
    $INSTALL_DIR =
        File::Spec->catpath((File::Spec->splitpath(__FILE__))[0, 1], q{});
    @XPL_PATH = ($INSTALL_DIR, File::Spec->curdir);

    # For now, I have an ugly hack in place to make the functionality that
    # runs under HTTP::Daemon/Net::Server work better with SSL. This flag
    # starts out true, then gets set to false the first time the hack is
    # applied, so that it doesn't get repeated over and over...
    $IO_SOCKET_SSL_HACK_NEEDED = 1;

    # Check for compression support
    $COMPRESSION_AVAILABLE =
        (eval { load Compress::Zlib; 1; }) ? 'deflate' : q{};

    # Set up the initial table of fault-types and their codes/messages
    %FAULT_TABLE = (
        badxml       => [ 100 => 'XML parse error: %s' ],
        badmethod    => [ 200 => 'Method lookup error: %s' ],
        badsignature => [ 201 => 'Method signature error: %s' ],
        execerror    => [ 300 => 'Code execution error: %s' ],
    );

    # This is used by add_method to map "types" to instantiation classes
    %CLASS_MAP = (
        method    => 'RPC::XML::Method',
        procedure => 'RPC::XML::Procedure',
        function  => 'RPC::XML::Function',
    );
}

$VERSION = '1.75';

###############################################################################
#
#   Sub Name:       new
#
#   Description:    Create a new RPC::XML::Server object. This entails getting
#                   a HTTP::Daemon object, saving several internal values, and
#                   other operations.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $class    in      scalar    Ref or string for the class
#                   %args     in      hash      Additional arguments
#
#   Returns:        Success:    object reference
#                   Failure:    error string
#
###############################################################################
sub new ## no critic (ProhibitExcessComplexity)
{
    my ($class, %args) = @_;

    my (
        $self, $http, $resp, $host, $port, $queue, $URI, $srv_version,
        $srv_name
    );

    # Don't accept a blessed value for $class
    if (ref $class)
    {
        return __PACKAGE__ . '::new: Must be called as a static method';
    }

    $self = bless {}, $class;

    $srv_version = delete $args{server_version} || $self->version;
    $srv_name    = delete $args{server_name}    || $class;
    $self->{__server_token} = "$srv_name/$srv_version";

    if (delete $args{no_http})
    {
        $self->{__host} = delete $args{host} || q{};
        $self->{__port} = delete $args{port} || q{};
    }
    else
    {
        require HTTP::Daemon;

        $host  = delete $args{host}  || q{};
        $port  = delete $args{port}  || q{};
        $queue = delete $args{queue} || 5;
        $http  = HTTP::Daemon->new(
            ReuseAddr => 1,
            ($host  ? (LocalHost => $host)  : ()),
            ($port  ? (LocalPort => $port)  : ()),
            ($queue ? (Listen    => $queue) : ())
        );
        if (! $http)
        {
            return "${class}::new: Unable to create HTTP::Daemon object: $@";
        }
        $URI              = URI->new($http->url);
        $self->{__host}   = $URI->host;
        $self->{__port}   = $URI->port;
        $self->{__daemon} = $http;
    }

    # Create and store the cached response object for later cloning and use
    $resp = HTTP::Response->new();
    $resp->header(
        # This is essentially the same string returned by the
        # default "identity" method that may be loaded from a
        # XPL file. But it hasn't been loaded yet, and may not
        # be, hence we set it here (possibly from option values)
        RPC_Server   => $self->{__server_token},
        RPC_Encoding => 'XML-RPC',
        # Set any other headers as well
        Accept => 'text/xml'
    );
    $resp->content_type('text/xml');
    $resp->code(RC_OK);
    $resp->message('OK');
    $self->{__response} = $resp;

    # Basic (scalar) properties
    $self->{__path}         = delete $args{path} || q{};
    $self->{__started}      = 0;
    $self->{__method_table} = {};
    $self->{__requests}     = 0;
    $self->{__auto_methods} = delete $args{auto_methods} || 0;
    $self->{__auto_updates} = delete $args{auto_updates} || 0;
    $self->{__debug}        = delete $args{debug} || 0;
    $self->{__xpl_path}     = delete $args{xpl_path} || [];
    $self->{__timeout}      = delete $args{timeout} || 10;
    $self->{__parser}       = RPC::XML::ParserFactory->new(
        $args{parser} ? @{delete $args{parser}} : ()
    );

    # Add the basic paths (content of @XPL_PATH) to our local XPL path
    push @{$self->{__xpl_path}}, @XPL_PATH;

    # Set up the default methods unless requested not to
    if (! delete $args{no_default})
    {
        $self->add_default_methods;
    }

    # Compression support
    if (delete $args{no_compress})
    {
        $self->{__compress} = q{};
    }
    else
    {
        $self->{__compress} = $COMPRESSION_AVAILABLE;
        # Add some more headers to the default response object for compression.
        # It looks wasteful to keep using the hash key, but it makes it easier
        # to change the string in just one place (above) if I have to.
        if ($self->{__compress})
        {
            $resp->header(Accept_Encoding => $self->{__compress});
        }
        $self->{__compress_thresh} = delete $args{compress_thresh} || 4096;
        # Yes, I know this is redundant. It's for future expansion/flexibility.
        $self->{__compress_re} =
            $self->{__compress} ? qr/$self->{__compress}/ : qr/deflate/;
    }

    # Parameters to control the point at which messages are shunted to temp
    # files due to size, and where to home the temp files. Start with a size
    # threshold of 1Meg and no specific dir (which will fall-through to the
    # tmpdir() method of File::Spec).
    $self->{__message_file_thresh} = delete $args{message_file_thresh} ||
        1_048_576;
    $self->{__message_temp_dir} = delete $args{message_temp_dir} || q{};

    # Set up the table of response codes/messages that will be used when the
    # server is sending a controlled error message to a client (as opposed to
    # something HTTP-level that is less within our control).
    $self->{__fault_table} = {};
    for my $fault (keys %FAULT_TABLE)
    {
        $self->{__fault_table}->{$fault} = [ @{$FAULT_TABLE{$fault}} ];
    }
    if ($args{fault_code_base})
    {
        my $base = delete $args{fault_code_base};
        # Apply the numerical offset to all (current) error codes
        for my $key (keys %{$self->{__fault_table}})
        {
            $self->{__fault_table}->{$key}->[0] += $base;
        }
    }
    if ($args{fault_table})
    {
        my $local_table = delete $args{fault_table};
        # Merge any data from this table into the object's fault-table
        for my $key (keys %{$local_table})
        {
            $self->{__fault_table}->{$key} = (ref $local_table->{$key}) ?
                [ @{$local_table->{$key}} ] : $local_table->{$key};
        }
    }

    # Copy the remaining args over untouched
    for (keys %args)
    {
        $self->{$_} = $args{$_};
    }

    return $self;
}

# Most of these tiny subs are accessors to the internal hash keys. They not
# only control access to the internals, they ease sub-classing.

sub version { return $VERSION }

sub INSTALL_DIR { return $INSTALL_DIR }

sub url
{
    my $self = shift;

    my $host;

    if ($self->{__daemon})
    {
        return $self->{__daemon}->url;
    }
    if (! ($host = $self->host))
    {
        return;
    }

    my $path = $self->path;
    my $port = $self->port;
    if ($port == 443)
    {
        return "https://$host$path";
    }
    elsif ($port == 80)
    {
        return "http://$host$path";
    }
    else
    {
        return "http://$host:$port$path";
    }
}

sub product_tokens
{
    my $self = shift;

    my $class = ref $self;
    $class ||= $self;
    return sprintf '%s/%s', $class, $self->version;
}

# This fetches/sets the internal "started" timestamp. Unlike the other
# plain-but-mutable attributes, this isn't set to the passed-value but
# rather a non-null argument sets it from the current time.
sub started
{
    my ($self, $set_started) = @_;

    my $old = $self->{__started} || 0;
    if ($set_started)
    {
        $self->{__started} = time;
    }

    return $old;
}

BEGIN
{
    no strict 'refs'; ## no critic (ProhibitNoStrict)

    # These are mutable member values for which the logic only differs in
    # the name of the field to modify:
    for my $method (qw(compress_thresh message_file_thresh message_temp_dir))
    {
        *{$method} = sub {
            my ($self, $value) = @_;

            my $old = $self->{"__$method"};
            if (defined $value)
            {
                $self->{"__$method"} = $value;
            }

            $old;
        }
    }

    # These are immutable member values, so this simple block applies to all
    for my $method (qw(path host port requests response compress compress_re
                       parser))
    {
        *{$method} = sub { shift->{"__$method"} }
    }
}

# Get/set the search path for XPL files
sub xpl_path
{
    my ($self, $path) = @_;
    my $ret  = $self->{__xpl_path};

    if ($path && ref $path eq 'ARRAY')
    {
        $self->{__xpl_path} = $path;
    }

    return $ret;
}

###############################################################################
#
#   Sub Name:       add_method
#
#   Description:    Add a funtion-to-method mapping to the server object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object to add to
#                   $meth     in      scalar    Hash ref of data or file name
#
#   Globals:        %CLASS_MAP
#
#   Returns:        Success:    $self
#                   Failure:    error string
#
###############################################################################
sub add_method
{
    my ($self, $meth) = @_;

    my $me = ref($self) . '::add_method';

    if (! ref $meth)
    {
        my $val = $self->method_from_file($meth);
        if (! ref $val)
        {
            return "$me: Error loading from file $meth: $val";
        }
        else
        {
            $meth = $val;
        }
    }
    elsif (ref $meth eq 'HASH')
    {
        # Make a copy of the contents of $meth, so we don't make permanent
        # changes:
        my %meth_copy = map { $_ => $meth->{$_} } (keys %{$meth});

        # If the type of this method is not set, default to "method". The
        # add_procedure and add_function calls should set this as needed.
        my $type = delete $meth_copy{type} || 'method';

        if (! (my $class = $CLASS_MAP{lc $type}))
        {
            return "$me: Unknown type: $type";
        }
        else
        {
            $meth = $class->new(\%meth_copy);
        }
    }
    elsif (! (blessed $meth and $meth->isa('RPC::XML::Procedure')))
    {
        return "$me: Method argument must be a file name, a hash " .
            'reference or an object derived from RPC::XML::Procedure';
    }

    $self->{__method_table}->{$meth->name} = $meth;

    return $self;
}

###############################################################################
#
#   Sub Name:       add_procedure
#
#   Description:    This filters through to add_method, but if the passed-in
#                   value is a hash reference forces the "type" to be
#                   "procedure".
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object reference
#                   $meth     in      scalar    Procedure to add
#
#   Returns:        threads through to add_method
#
###############################################################################
sub add_procedure
{
    my ($self, $meth) = @_;

    # Anything else but a hash-reference goes through unaltered
    if (ref($meth) eq 'HASH')
    {
        $meth->{type} = 'procedure';
    }

    return $self->add_method($meth);
}

###############################################################################
#
#   Sub Name:       add_function
#
#   Description:    This filters through to add_method, but if the passed-in
#                   value is a hash reference forces the "type" to be
#                   "function".
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object reference
#                   $meth     in      scalar    Procedure to add
#
#   Returns:        threads through to add_method
#
###############################################################################
sub add_function
{
    my ($self, $meth) = @_;

    # Anything else but a hash-reference goes through unaltered
    if (ref($meth) eq 'HASH')
    {
        $meth->{type} = 'function';
    }

    return $self->add_method($meth);
}

###############################################################################
#
#   Sub Name:       method_from_file
#
#   Description:    Create a RPC::XML::Procedure (or ::Method) object from the
#                   passed-in file name, using the object's search path if the
#                   name is not already absolute.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $file     in      scalar    Name of file to load
#
#   Returns:        Success:    Method-object reference
#                   Failure:    error message
#
###############################################################################
sub method_from_file
{
    my ($self, $file) = @_;

    if (! File::Spec->file_name_is_absolute($file))
    {
        my $path;
        for my $dir (@{$self->xpl_path})
        {
            $path = File::Spec->catfile($dir, $file);
            if (-f $path)
            {
                $file = File::Spec->canonpath($path);
                last;
            }
        }
    }
    # Just in case it still didn't appear in the path, we really want an
    # absolute path:
    if (! File::Spec->file_name_is_absolute($file))
    {
        $file = File::Spec->rel2abs($file);
    }

    # When reading a XPL file, RPC::XML::Procedure->new() acts sort of like a
    # factory constructor, returning the type of object the XPL file specifies
    # even when that isn't RPC::XML::Procedure.
    return RPC::XML::Procedure->new($file);
}

###############################################################################
#
#   Sub Name:       get_method
#
#   Description:    Get the current binding for the remote-side method $name.
#                   Returns undef if the method is not defined for the server
#                   instance.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Class instance
#                   $name     in      scalar    Name of the method being looked
#                                                 up
#
#   Returns:        Success:    Method-class reference
#                   Failure:    error string
#
###############################################################################
sub get_method
{
    my ($self, $name) = @_;

    my $meth = $self->{__method_table}->{$name};
    if (! defined $meth)
    {
        if ($self->{__auto_methods})
        {
            # Try to load this dynamically on the fly, from any of the dirs
            # that are in this object's @xpl_path
            (my $loadname = $name) =~ s/^system[.]//;
            $self->add_method("$loadname.xpl");
        }
        # If method is still not in the table, we were unable to load it
        if (! ($meth = $self->{__method_table}->{$name}))
        {
            return "Unknown method: $name";
        }
    }
    # Check the mod-time of the file the method came from, if the test is on
    if ($self->{__auto_updates} &&
        $meth->{file} &&
        ($meth->{mtime} < (stat $meth->{file})[9]))
    {
        my $ret = $meth->reload;
        if (! ref $ret)
        {
            return "Reload of method $name failed: $ret";
        }
    }

    return $meth;
}

# For name-symmetry:
*get_procedure = *get_function = \&get_method;

###############################################################################
#
#   Sub Name:       server_loop
#
#   Description:    Enter a server-loop situation, using the accept() loop of
#                   HTTP::Daemon if $self has such an object, or falling back
#                   Net::Server otherwise.
#
#                   The critic disabling is because we may manipulate @_
#                   when using Net::Server.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   %args     in      hash      Additional parameters to set up
#                                                 before calling the superclass
#                                                 Run method
#
#   Returns:        string if error, otherwise void
#
###############################################################################
sub server_loop ## no critic (RequireArgUnpacking,ProhibitExcessComplexity)
{
    my $self = shift;

    if ($self->{__daemon})
    {
        my ($conn, $req, $resp, $reqxml, $respxml, $exit_now, $timeout,
            $eval_return);

        my %args = @_;

        # Localize and set the signal handler as an exit route
        my @exit_signals;

        if (exists $args{signal} and $args{signal} ne 'NONE')
        {
            @exit_signals =
                (ref $args{signal}) ? @{$args{signal}} : $args{signal};
        }
        else
        {
            push @exit_signals, 'INT';
        }

        local @SIG{@exit_signals} = (sub { $exit_now++ }) x @exit_signals;

        $self->started('set');
        $exit_now = 0;
        $timeout  = $self->{__daemon}->timeout(1);
        while (! $exit_now)
        {
            $conn = $self->{__daemon}->accept;

            if ($exit_now)
            {
                last;
            }
            if (! $conn)
            {
                next;
            }
            $conn->timeout($self->timeout);
            $self->process_request($conn);

            $eval_return = eval {
                local $SIG{PIPE} = sub { die "server_loop: Caught SIGPIPE\n"; };
                $conn->close;
                1;
            };
            if ((! $eval_return) && $@)
            {
                warn "Cannot close connection: $@\n";
            }

            undef $conn;    # Free up any lingering resources
        }

        if (defined $timeout)
        {
            $self->{__daemon}->timeout($timeout);
        }
    }
    else
    {
        # This is the Net::Server block, but for now HTTP::Daemon is needed
        # for the code that converts socket data to a HTTP::Request object
        require HTTP::Daemon;

        my $conf_file_flag = 0;
        my $port_flag      = 0;
        my $host_flag      = 0;

        # Disable critic on the C-style for-loop because we need to step by
        # 2 as we check for Net::Server arguments...
        for (my $i = 0; $i < @_; $i += 2) ## no critic (ProhibitCStyleForLoops)
        {
            if ($_[$i] eq 'conf_file') { $conf_file_flag = 1; }
            if ($_[$i] eq 'port')      { $port_flag      = 1; }
            if ($_[$i] eq 'host')      { $host_flag      = 1; }
        }

        # An explicitly-given conf-file trumps any specified at creation
        if (exists($self->{conf_file}) and (!$conf_file_flag))
        {
            push @_, 'conf_file', $self->{conf_file};
            $conf_file_flag = 1;
        }

        # Don't do this next part if they've already given a port, or are
        # pointing to a config file:
        if (! ($conf_file_flag || $port_flag))
        {
            push @_, 'port', $self->{port} || $self->port || 9000;
            push @_, 'host', $self->{host} || $self->host || q{*};
        }

        # Try to load the Net::Server::MultiType module
        if (! eval { require Net::Server::MultiType; 1; })
        {
            if ($@)
            {
                return ref($self) .
                    "::server_loop: Error loading Net::Server::MultiType: $@";
            }
        }
        unshift @RPC::XML::Server::ISA, 'Net::Server::MultiType';

        $self->started('set');
        # ...and we're off!
        $self->run(@_);
    }

    return;
}

###############################################################################
#
#   Sub Name:       post_configure_loop
#
#   Description:    Called by the Net::Server classes after all the config
#                   steps have been done and merged.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Class object
#
#   Returns:        $self
#
###############################################################################
sub post_configure_hook
{
    my $self = shift;

    $self->{__host} = $self->{server}->{host};
    $self->{__port} = $self->{server}->{port};

    return $self;
}

###############################################################################
#
#   Sub Name:       pre_loop_hook
#
#   Description:    Called by Net::Server classes after the post_bind method,
#                   but before the socket-accept loop starts.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object instance
#
#   Returns:        $self
#
###############################################################################
sub pre_loop_hook
{
    my $self = shift;

    # We have to disable the __DIE__ handler for the sake of XML::Parser::Expat
    $SIG{__DIE__} = q{}; ## no critic (RequireLocalizedPunctuationVars)

    return $self;
}

###############################################################################
#
#   Sub Name:       process_request
#
#   Description:    This is provided for the case when we run as a subclass
#                   of Net::Server.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       This class object
#                   $conn     in      ref       If present, it's a connection
#                                                 object from HTTP::Daemon
#
#   Returns:        void
#
###############################################################################
sub process_request ## no critic (ProhibitExcessComplexity)
{
    my $self = shift;
    my $conn = shift;

    my (
        $req,     $reqxml,     $resp,       $respxml,  $do_compress,
        $parser,  $com_engine, $length,     $read,     $buf,
        $resp_fh, $tmpdir,     $peerfamily, $peeraddr, $peerhost, $peerport,
    );

    my $me = ref($self) . '::process_request';
    if (! $conn)
    {
        # Maintain compatibility with Net::Server 0.99, which does not pass
        # the connection object at all:
        $conn = $self->{server}->{client};
    }
    if (ref($conn) =~ /^Net::Server::Proto/)
    {
        bless $conn, 'HTTP::Daemon::ClientConn';
        ${*{$conn}}{'httpd_daemon'} = $self;

        if ($IO::Socket::SSL::VERSION &&
            $RPC::XML::Server::IO_SOCKET_SSL_HACK_NEEDED)
        {
            no strict 'vars'; ## no critic (ProhibitNoStrict)
            # RT 43019: Don't do this if Socket6/IO::Socket::INET6 is in
            # effect, as it causes calls to unpack_sockaddr_in6 to break.
            if (! (defined $Socket6::VERSION ||
                   defined $IO::Socket::INET6::VERSION))
            {
                unshift @HTTP::Daemon::ClientConn::ISA, 'IO::Socket::SSL';
            }

            $RPC::XML::Server::IO_SOCKET_SSL_HACK_NEEDED = 0;
        }
    }

    # These will be attached to any and all request objects that are
    # (successfully) read from $conn.
    $peerfamily = $conn->sockdomain;
    $peeraddr = $conn->peeraddr;
    $peerport = $conn->peerport;
    $peerhost = $conn->peerhost;
    while ($conn and $req = $conn->get_request('headers only'))
    {
        if ($req->method eq 'HEAD')
        {
            # The HEAD method will be answered with our return headers,
            # both as a means of self-identification and a verification
            # of live-status. All the headers were pre-set in the cached
            # HTTP::Response object. Also, we don't count this for stats.
            $conn->send_response($self->response);
        }
        elsif ($req->method eq 'POST')
        {
            # Get a XML::Parser::ExpatNB object
            $parser = $self->parser->parse();

            $do_compress = 0; # in case it was set for a previous response
            if (($req->content_encoding || q{}) =~ $self->compress_re)
            {
                if (! $self->compress)
                {
                    $conn->send_error(RC_BAD_REQUEST,
                        "$me: Compression not permitted in " . 'requests');
                    next;
                }

                $do_compress = 1;
            }

            if (($req->content_encoding || q{}) =~ /chunked/i)
            {
                # Technically speaking, we're not supposed to honor chunked
                # transfer-encoding...
                croak "$me: 'chunked' content-encoding not (yet) supported";
            }
            else
            {
                $length = $req->content_length;
                if ($do_compress)
                {
                    # Spin up the compression engine
                    if (! ($com_engine = Compress::Zlib::inflateInit()))
                    {
                        $conn->send_error(RC_INTERNAL_SERVER_ERROR,
                            "$me: Unable to initialize the " .
                                'Compress::Zlib engine');
                        next;
                    }
                }

                $buf = q{};
                while ($length > 0)
                {
                    if ($buf = $conn->read_buffer)
                    {
                        # Anything that get_request read, but didn't use, was
                        # left in the read buffer. The call to sysread() should
                        # NOT be made until we've emptied this source, first.
                        $read = length $buf;
                        $conn->read_buffer(q{}); # Clear it, now that it's read
                    }
                    else
                    {
                        $read = sysread $conn, $buf,
                            ($length < 2048) ? $length : 2048;
                        if (! $read)
                        {
                            # Convert this print to a logging-hook call.
                            # Umm, when I have real logging hooks, I mean.
                            # The point is, odds are very good that $conn is
                            # dead to us now, and I don't want this package
                            # taking over SIGPIPE as well as the ones it
                            # already monopolizes.
                            #print STDERR "Error: Connection Dropped\n";
                            return;
                        }
                    }
                    $length -= $read;
                    if ($do_compress)
                    {
                        if (! ($buf = $com_engine->inflate($buf)))
                        {
                            $conn->send_error(RC_INTERNAL_SERVER_ERROR,
                                "$me: Error inflating " . 'compressed data');
                            # This error also means that even if Keep-Alive
                            # is set, we don't know how much of the stream
                            # is corrupted.
                            $conn->force_last_request;
                            next;
                        }
                    }

                    if (! eval { $parser->parse_more($buf); 1; })
                    {
                        if ($@)
                        {
                            $conn->send_error(
                                RC_INTERNAL_SERVER_ERROR,
                                "$me: Parse error in (compressed) " .
                                "XML request (mid): $@"
                            );
                            # Again, the stream is likely corrupted
                            $conn->force_last_request;
                            next;
                        }
                    }
                }

                if (! eval { $reqxml = $parser->parse_done(); 1; })
                {
                    if ($@)
                    {
                        $conn->send_error(RC_INTERNAL_SERVER_ERROR,
                                          "$me: Parse error in (compressed) " .
                                          "XML request (end): $@");
                        next;
                    }
                }
            }

            # Dispatch will always return a RPC::XML::response.
            # RT29351: If there was an error from RPC::XML::ParserFactory
            # (such as a message that didn't conform to spec), then return it
            # directly as a fault, don't have dispatch() try and handle it.
            if (ref $reqxml)
            {
                # Set localized keys on $self, based on the connection info
                ## no critic (ProhibitLocalVars)
                local $self->{peerfamily} = $peerfamily;
                local $self->{peeraddr} = $peeraddr;
                local $self->{peerhost} = $peerhost;
                local $self->{peerport} = $peerport;
                local $self->{request}  = $req;
                $respxml = $self->dispatch($reqxml);
            }
            else
            {
                $respxml = RPC::XML::response->new(
                    $self->server_fault('badxml', $reqxml));
            }

            # Clone the pre-fab response and set headers
            $resp = $self->response->clone;
            # Should we apply compression to the outgoing response?
            $do_compress = 0;    # In case it was set above for incoming data
            if ($self->compress &&
                ($respxml->length > $self->compress_thresh) &&
                (($req->header('Accept-Encoding') || q{}) =~
                 $self->compress_re))
            {
                $do_compress = 1;
                $resp->header(Content_Encoding => $self->compress);
            }
            # Next step, determine the response disposition. If it is above the
            # threshold for a requested file cut-off, send it to a temp file
            if ($self->message_file_thresh &&
                $self->message_file_thresh < $respxml->length)
            {
                # Start by creating a temp-file
                $tmpdir = $self->message_temp_dir || File::Spec->tmpdir;
                # File::Temp->new() croaks on error
                $resp_fh =
                    eval { File::Temp->new(UNLINK => 1, DIR => $tmpdir) };
                if (! $resp_fh)
                {
                    $conn->send_error(
                        RC_INTERNAL_SERVER_ERROR,
                        "$me: Error opening tmpfile: $@"
                    );
                    next;
                }
                # Make it auto-flush
                $resp_fh->autoflush();

                # Now that we have it, spool the response to it. This is a
                # little hairy, since we still have to allow for compression.
                # And though the response could theoretically be HUGE, in
                # order to compress we have to write it to a second temp-file
                # first, so that we can compress it into the primary handle.
                if ($do_compress)
                {
                    my $fh_compress =
                        eval { File::Temp->new(UNLINK => 1, DIR => $tmpdir) };
                    if (! $fh_compress)
                    {
                        $conn->send_error(
                            RC_INTERNAL_SERVER_ERROR,
                            "$me: Error opening compression tmpfile: $@"
                        );
                        next;
                    }
                    # Make it auto-flush
                    $fh_compress->autoflush();

                    # Write the request to the second FH
                    $respxml->serialize($fh_compress);
                    seek $fh_compress, 0, 0;

                    # Spin up the compression engine
                    if (! ($com_engine = Compress::Zlib::deflateInit()))
                    {
                        $conn->send_error(RC_INTERNAL_SERVER_ERROR,
                            "$me: Unable to initialize the " .
                                'Compress::Zlib engine');
                        next;
                    }

                    # Spool from the second FH through the compression engine,
                    # into the intended FH.
                    $buf = q{};
                    my $out;
                    while (read $fh_compress, $buf, 4096)
                    {
                        if (! defined($out = $com_engine->deflate(\$buf)))
                        {
                            $conn->send_error(RC_INTERNAL_SERVER_ERROR,
                                "$me: Compression failure in " . 'deflate()');
                            next;
                        }
                        print {$resp_fh} $out;
                    }
                    # Make sure we have all that's left
                    if (! defined($out = $com_engine->flush))
                    {
                        $conn->send_error(RC_INTERNAL_SERVER_ERROR,
                            "$me: Compression flush failure in deflate()");
                        next;
                    }
                    print {$resp_fh} $out;

                    # Close the secondary FH. Rewinding the primary is done
                    # later.
                    if (! close $fh_compress)
                    {
                        carp "Error closing temp file: $!";
                    }
                }
                else
                {
                    $respxml->serialize($resp_fh);
                }
                seek $resp_fh, 0, 0;

                $resp->content_length(-s $resp_fh);
                $resp->content(
                    sub {
                        my $buffer = q{};
                        if (! defined(read $resp_fh, $buffer, 4096))
                        {
                            return;
                        }
                        $buffer;
                    }
                );
            }
            else
            {
                # Treat the content strictly in-memory
                utf8::encode($buf = $respxml->as_string);
                if ($do_compress)
                {
                    $buf = Compress::Zlib::compress($buf);
                }
                $resp->content($buf);
                # With $buf force-downgraded to octets, length() should work
                $resp->content_length(length $buf);
            }

            my $eval = eval {
                local $SIG{PIPE} = sub { die "Caught SIGPIPE\n"; };
                $conn->send_response($resp);
                1;
            };
            if (! $eval && $@ && $@ =~ /Caught SIGPIPE/)
            {
                # Client disconnected, maybe even before we started sending
                # our response. Either way, $conn is useless now.
                undef $conn;
            }
            undef $resp;
        }
        else
        {
            $conn->send_error(RC_FORBIDDEN);
        }
    }

    return;
}

###############################################################################
#
#   Sub Name:       dispatch
#
#   Description:    Route the request by parsing it, determining what the
#                   Perl routine should be, etc.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $xml      in      ref       Reference to the XML text, or
#                                                 a RPC::XML::request object.
#                                                 If it is a listref, assume
#                                                 [ name, @args ].
#                   $reftable in      hashref   If present, a reference to the
#                                                 current-running table of
#                                                 back-references
#
#   Returns:        RPC::XML::response object
#
###############################################################################
sub dispatch
{
    my ($self, $xml) = @_;

    my ($reqobj, @args, $response, $name, $meth);

    if (ref $xml eq 'SCALAR')
    {
        $reqobj = $self->parser->parse(${$xml});
        if (! ref $reqobj)
        {
            return RPC::XML::response->
                new($self->server_fault(badxml => $reqobj));
        }
    }
    elsif (ref $xml eq 'ARRAY')
    {
        # This is sort of a cheat, to make the system.multicall API call a
        # lot easier. The syntax isn't documented in the manual page, for good
        # reason.
        $reqobj = RPC::XML::request->new(@{$xml});
    }
    elsif (blessed $xml && $xml->isa('RPC::XML::request'))
    {
        $reqobj = $xml;
    }
    else
    {
        $reqobj = $self->parser->parse($xml);
        if (! ref $reqobj)
        {
            return RPC::XML::response->
                new($self->server_fault(badxml => $reqobj));
        }
    }

    @args = @{$reqobj->args};
    $name = $reqobj->name;

    # Get the method, call it, and bump the internal requests counter. Create
    # a fault object if there is problem with the method object itself.
    $meth = $self->get_method($name);
    if (ref $meth)
    {
        $response = $meth->call($self, @args);
        if (! (($name eq 'system.status') &&
               @args &&
               ($args[0]->type eq 'boolean') &&
               ($args[0]->value)))
        {
            $self->{__requests}++;
        }
    }
    else
    {
        $response = $self->server_fault(
            badmethod => "No method '$meth' on server"
        );
    }

    # All the eval'ing and error-trapping happened within the method class
    return RPC::XML::response->new($response);
}

###############################################################################
#
#   Sub Name:       call
#
#   Description:    This is an internal, end-run-around-dispatch() method to
#                   allow the RPC methods that this server has and knows about
#                   to call each other through their reference to the server
#                   object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $name     in      scalar    Name of the method to call
#                   @args     in      list      Arguments (if any) to pass
#
#   Returns:        Success:    return value of the call
#                   Failure:    error string
#
###############################################################################
sub call
{
    my ($self, $name, @args) = @_;

    my $meth;

    # Two VERY important notes here: The values in @args are not pre-treated
    # in any way, so not only should the receiver understand what they're
    # getting, there's no signature checking taking place, either.
    #
    # Second, if the normal return value is not distinguishable from a string,
    # then the caller may not recognize if an error occurs.

    $meth = $self->get_method($name);
    if (! ref $meth)
    {
        return $meth;
    }

    return $meth->call($self, @args);
}

###############################################################################
#
#   Sub Name:       add_default_methods
#
#   Description:    This adds all the methods that were shipped with this
#                   package, by threading through to add_methods_in_dir()
#                   with the global constant $INSTALL_DIR.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object reference/static class
#                   @details  in      ref       Details of names to add or skip
#
#   Returns:        $self
#
###############################################################################
sub add_default_methods
{
    my ($self, @details) = @_;

    return $self->add_methods_in_dir($self->INSTALL_DIR, @details);
}

###############################################################################
#
#   Sub Name:       add_methods_in_dir
#
#   Description:    This adds all methods specified in the directory passed,
#                   in accordance with the details specified.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Class instance
#                   $dir      in      scalar    Directory to scan
#                   @details  in      list      Possible hanky-panky with the
#                                                 list of methods to install
#
#   Returns:        $self
#
###############################################################################
sub add_methods_in_dir
{
    my ($self, $dir, @details) = @_;

    my $negate = 0;
    my $detail = 0;
    my (%details, $ret);

    if (@details)
    {
        $detail = 1;
        if ($details[0] =~ /^-?except/i)
        {
            $negate = 1;
            shift @details;
        }
        for (@details)
        {
            if (! /[.]xpl$/)
            {
                $_ .= '.xpl';
            }
        }
        @details{@details} = (1) x @details;
    }

    my $dh;
    if (! opendir $dh, $dir)
    {
        return "Error opening $dir for reading: $!";
    }
    my @files = grep { $_ =~ /[.]xpl$/ } readdir $dh;
    closedir $dh;

    for my $file (@files)
    {
        # Use $detail as a short-circuit to avoid the other tests when we can
        if ($detail &&
            ($negate ? $details{$file} : ! $details{$file}))
        {
            next;
        }
        # n.b.: Giving the full path keeps add_method from having to search
        $ret = $self->add_method(File::Spec->catfile($dir, $file));
        if (! ref $ret)
        {
            return $ret;
        }
    }

    return $self;
}

# For name-symmetry:
*add_procedures_in_dir = *add_functions_in_dir = \&add_methods_in_dir;

###############################################################################
#
#   Sub Name:       delete_method
#
#   Description:    Remove any current binding for the named method on the
#                   calling server object. Note that if this method is shared
#                   across other server objects, it won't be destroyed until
#                   the last server deletes it.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $name     in      scalar    Name of method to lost
#
#   Returns:        Success:    $self
#                   Failure:    error message
#
###############################################################################
sub delete_method
{
    my ($self, $name) = @_;

    if ($name)
    {
        if ($self->{__method_table}->{$name})
        {
            delete $self->{__method_table}->{$name};
        }
    }
    else
    {
        return ref $self . "::delete_method: No such method $name";
    }

    return $self;
}

# For name-symmetry:
*delete_procedure = *delete_function = \&delete_method;

###############################################################################
#
#   Sub Name:       list_methods
#
#   Description:    Return a list of the methods this object has published.
#                   Returns the names, not the objects.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#
#   Returns:        List of names, possibly empty
#
###############################################################################
sub list_methods
{
    return keys %{shift->{__method_table}};
}

# For name-symmetry:
*list_procedures = *list_functions = \&list_methods;

###############################################################################
#
#   Sub Name:       share_methods
#
#   Description:    Share the named methods as found on $src_srv into the
#                   method table of the calling object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $src_srv  in      ref       Another object of this class
#                   @names    in      list      One or more method names
#
#   Returns:        Success:    $self
#                   Failure:    error message
#
###############################################################################
sub share_methods
{
    my ($self, $src_srv, @names) = @_;

    my ($me, $pkg, %methods, @methods, $meth, @list, @missing);

    $me  = ref($self) . '::share_methods';
    $pkg = __PACKAGE__; # So it can go inside quoted strings

    if (! (blessed $src_srv && $src_srv->isa($pkg)))
    {
        return "$me: First arg not derived from $pkg, cannot share";
    }
    if (! @names)
    {
        return "$me: Must specify at least one method name for sharing";
    }

    # Scan @names for any regex objects, and if found insert the matches into
    # the list.
    #
    # Only do this once:
    @methods = keys %{$src_srv->{__method_table}};
    for my $name (@names)
    {
        if (ref $name eq 'Regexp')
        {
            for (grep { $_ =~ $name } @methods)
            {
                $methods{$_}++;
            }
        }
        else
        {
            $methods{$name}++;
        }
    }
    # This has the benefit of trimming any redundancies caused by regex's
    @names = keys %methods;

    # Note that the method refs are saved until we've verified all of them.
    # If we have to return a failure message, I don't want to leave a half-
    # finished job or have to go back and undo (n-1) additions because of one
    # failure.
    for (@names)
    {
        $meth = $src_srv->get_method($_);
        if (ref $meth)
        {
            push @list, $meth;
        }
        else
        {
            push @missing, $_;
        }
    }

    if (@missing)
    {
        return "$me: One or more methods not found on source object: " .
            join q{ } => @missing;
    }
    else
    {
        for (@list)
        {
            $self->add_method($_);
        }
    }

    return $self;
}

# For name-symmetry:
*share_procedures = *share_functions = \&share_methods;

###############################################################################
#
#   Sub Name:       copy_methods
#
#   Description:    Copy the named methods as found on $src_srv into the
#                   method table of the calling object. This differs from
#                   share() above in that only the coderef is shared, the
#                   rest of the method is a completely new object.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $src_srv  in      ref       Another object of this class
#                   @names    in      list      One or more method names
#
#   Returns:        Success:    $self
#                   Failure:    error message
#
###############################################################################
sub copy_methods
{
    my ($self, $src_srv, @names) = @_;

    my ($me, $pkg, %methods, @methods, $meth, @list, @missing);

    $me  = ref($self) . '::copy_methods';
    $pkg = __PACKAGE__; # So it can go inside quoted strings

    if (! (blessed $src_srv && $src_srv->isa($pkg)))
    {
        return "$me: First arg not derived from $pkg, cannot copy";
    }
    if (! @names)
    {
        return "$me: Must specify at least one method name/regex for copying";
    }

    # Scan @names for any regez objects, and if found insert the matches into
    # the list.
    #
    # Only do this once:
    @methods = keys %{$src_srv->{__method_table}};
    for my $name (@names)
    {
        if (ref $name eq 'Regexp')
        {
            for (grep { $_ =~ $name } @methods)
            {
                $methods{$_}++;
            }
        }
        else
        {
            $methods{$name}++;
        }
    }
    # This has the benefit of trimming any redundancies caused by regex's
    @names = keys %methods;

    # Note that the method clones are saved until we've verified all of them.
    # If we have to return a failure message, I don't want to leave a half-
    # finished job or have to go back and undo (n-1) additions because of one
    # failure.
    for (@names)
    {
        $meth = $src_srv->get_method($_);
        if (ref $meth)
        {
            push @list, $meth->clone;
        }
        else
        {
            push @missing, $_;
        }
    }

    if (@missing)
    {
        return "$me: One or more methods not found on source object: @missing";
    }
    else
    {
        for (@list)
        {
            $self->add_method($_);
        }
    }

    return $self;
}

# For name-symmetry:
*copy_procedures = *copy_functions = \&copy_methods;

###############################################################################
#
#   Sub Name:       timeout
#
#   Description:    This sets the timeout for processing connections after
#                   a new connection has been accepted.  It returns the old
#                   timeout value.  If you pass in no value, it returns
#                   the current timeout.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object reference/static class
#                   $timeout  in      ref       New timeout value
#
#   Returns:        $self->{__timeout}
#
###############################################################################
sub timeout
{
    my ($self, $timeout) = @_;

    my $old_timeout = $self->{__timeout};
    if ($timeout)
    {
        $self->{__timeout} = $timeout;
    }

    return $old_timeout;
}

###############################################################################
#
#   Sub Name:       server_fault
#
#   Description:    Create a RPC::XML::fault object for the class of error
#                   and specific message that are passed in.
#
#   Arguments:      NAME      IN/OUT  TYPE      DESCRIPTION
#                   $self     in      ref       Object of this class
#                   $err      in      scalar    Type of error/fault to generate
#                   $message  in      scalar    Error text for the fault
#
#   Returns:        RPC::XML::fault instance
#
###############################################################################
sub server_fault
{
    my ($self, $err, $message) = @_;
    $message ||= q{}; # Avoid any "undef" warnings

    my ($code, $text);

    if (my $fault = $self->{__fault_table}->{$err})
    {
        if (ref $fault)
        {
            # This specifies both code and message
            ($code, $text) = @{$fault};
            # Replace (the first) "%s" with $message
            $text =~ s/%s/$message/;
        }
        else
        {
            # This is just the code, use $message verbatim
            ($code, $text) = ($fault, $message);
        }
    }
    else
    {
        $code = -1;
        $text = "Unknown error class '$err' (message is '$message')";
    }

    return RPC::XML::fault->new($code, $text);
}

1;

__END__

=head1 NAME

RPC::XML::Server - A server base-class for XML-RPC

=head1 SYNOPSIS

    use RPC::XML::Server;

    ...
    $srv = RPC::XML::Server->new(port => 9000);
    # Several of these, most likely:
    $srv->add_method(...);
    ...
    $srv->server_loop; # Never returns

=head1 DESCRIPTION

This is both a base-class for developing XML-RPC servers, and a working server
class in its own right. It is built upon the B<RPC::XML> data classes, and
defaults to using B<HTTP::Daemon> for the communication layer.

=head1 SUBROUTINES/METHODS

Use of the B<RPC::XML::Server> is based on an object model. A server is
instantiated from the class, methods (subroutines) are made public by adding
them through the object interface, and then the server object is responsible
for dispatching requests (and possibly for the HTTP listening, as well).

=head2 Static Methods

These methods are static to the package, and are used to provide external
access to internal settings:

=over 4

=item INSTALL_DIR

Returns the directory that this module is installed into. This is used by
methods such as B<add_default_methods> to locate the XPL files that are
shipped with the distribution.

=item version

Returns the version string associated with this package.

=item product_tokens

This returns the identifying string for the server, in the format
C<NAME/VERSION> consistent with other applications such as Apache and
B<LWP>. It is provided here as part of the compatibility with B<HTTP::Daemon>
that is required for effective integration with B<Net::Server>.

=back

=head2 Methods

The following are object (non-static) methods. Unless otherwise explicitly
noted, all methods return the invoking object reference upon success, and a
non-reference error string upon failure.

See L</Content Compression> below for details of how the server class manages
gzip-based compression and expansion of messages.

=over 4

=item new(OPTIONS)

Creates a new object of the class and returns the blessed reference. Depending
on the options, the object will contain some combination of an HTTP listener, a
pre-populated B<HTTP::Response> object, a B<RPC::XML::ParserFactory>-generated
object, and a dispatch table with the set of default procedures pre-loaded. The
options that B<new> accepts are passed as a hash of key/value pairs (not a hash
reference).  The accepted options are:

=over 4

=item B<no_http>

If passed with a C<true> value, prevents the creation and storage of the
B<HTTP::Daemon> object. This allows for deployment of a server object in other
environments. Note that if this is set, the B<server_loop> method described
below will silently attempt to use the B<Net::Server> module.

=item B<no_default>

If passed with a C<true> value, prevents the loading of the default procedures
provided with the B<RPC::XML> distribution. These may be later loaded using
the B<add_default_methods> interface described later. The procedures themselves
are described below (see L<"The Default Procedures Provided">).

=item B<path>

=item B<host>

=item B<port>

=item B<queue>

These four are specific to the HTTP-based nature of the server.  The B<path>
argument sets the additional URI path information that clients would use to
contact the server.  Internally, it is not used except in outgoing status and
introspection reports.  The B<host>, B<port> and B<queue> arguments are passed
to the B<HTTP::Daemon> constructor if they are passed. They set the hostname,
TCP/IP port, and socket listening queue, respectively. They may also be used
if the server object tries to use B<Net::Server> as an alternative server
core.

=item B<xpl_path>

If you plan to add procedures/methods/functions to the server object by passing
filenames to the B<add_method>/B<add_procedure>/B<add_function> calls, this
argument may be used to specify one or more additional directories to be
searched when the passed-in filename is a relative path. The value for this
must be an array reference. See also the B<add_*> and B<xpl_path> methods,
below.

=item B<timeout>

Specify a value (in seconds) for the B<HTTP::Daemon> server to use as a
timeout value when reading request data from an inbound connection. The
default value is 10 seconds. This value is not used except by B<HTTP::Daemon>.

=item B<auto_methods>

If specified and set to a true value, enables the automatic searching for a
requested remote method/procedure/function that is unknown to the server object
handling the request. If set to "no" (or not set at all), then a request for an
unknown function causes the object instance to report an error. If the routine
is still not found, the error is reported. Enabling this is a security risk,
and should only be permitted by a server administrator with fully informed
acknowledgement and consent.

=item B<auto_updates>

If specified and set to a "true" value, enables the checking of the
modification time of the file from which a method/procedure/function was
originally loaded. If the file has changed, the method is re-loaded before
execution is handed off. As with the auto-loading of methods, this represents a
security risk, and should only be permitted by a server administrator with
fully informed acknowledgement and consent.

=item B<parser>

If this parameter is passed, its value is expected to be an array
reference. The contents of that array are passed to the B<new> method of the
B<RPC::XML::ParserFactory> class, which creates the parser object that the
server object caches for its use.  See the B<RPC::XML::ParserFactory> manual
page for a list of recognized parameters to the constructor.

=item B<message_file_thresh>

If this key is passed, the value associated with it is assumed to be a
numerical limit to the size of in-memory messages. Any out-bound request that
would be larger than this when stringified is instead written to an anonynous
temporary file, and spooled from there instead. This is useful for cases in
which the request includes B<RPC::XML::base64> objects that are themselves
spooled from file-handles. This test is independent of compression, so even
if compression of a request would drop it below this threshold, it will be
spooled anyway. The file itself is created via File::Temp with C<UNLINK> set,
so once it is freed the disk space is immediately freed.

=item B<message_temp_dir>

If a message is to be spooled to a temporary file, this key can define a
specific directory in which to open those files. If this is not given, then
the C<tmpdir> method from the B<File::Spec> package is used, instead.

=item B<fault_code_base>

Specify a base integer value that is added to the numerical codes for all
faults the server can return. See L</"Server Faults"> for the list of faults
that are built-in to the server class. This allows an application to "move"
the B<RPC::XML::Server> pre-defined fault codes out of the way of codes that
the application itself may generate.

Note that this value is B<not> applied to any faults specified via the next
option, C<fault_table>. It is assumed that the developer has already applied
any offset to those codes.

=item B<fault_table>

Specify one or more fault types to either add to or override the built-in set
of faults for the server object. The value of this parameter is a hash
reference whose keys are the fault type and whose values are either a scalar
(which is taken to be the numerical code) or a list reference with two elements
(the code followed by the string). See L</"Server Faults"> for the list of
faults that are built-in to the server class, and for more information on
defining your own.

=back

Any other keys in the options hash not explicitly used by the constructor are
copied over verbatim onto the object, for the benefit of sub-classing this
class. All internal keys are prefixed with C<__> to avoid confusion. Feel
free to use this prefix only if you wish to re-introduce confusion.

=item url

This returns the HTTP URL that the server will be responding to, when it is in
the connection-accept loop. If the server object was created without a
built-in HTTP listener, then this method returns C<undef>.

=item requests

Returns the number of requests this server object has marshalled. Note that in
multi-process environments (such as Apache or Net::Server::PreFork) the value
returned will only reflect the messages dispatched by the specific process
itself.

=item response

Each instance of this class (and any subclasses that do not completely
override the C<new> method) creates and stores an instance of
B<HTTP::Response>, which is then used by the B<HTTP::Daemon> or B<Net::Server>
processing loops in constructing the response to clients. The response object
has all common headers pre-set for efficiency. This method returns a reference
to that object.

=item started([BOOL])

Gets and possibly sets the clock-time when the server starts accepting
connections. If a value is passed that evaluates to true, then the current
clock time is marked as the starting time. In either case, the current value
is returned. The clock-time is based on the internal B<time> command of Perl,
and thus is represented as an integer number of seconds since the system
epoch. Generally, it is suitable for passing to either B<localtime> or to the
C<time2iso8601> routine exported by the B<RPC::XML> package.

=item timeout(INT)

You can call this method to set the timeout of new connections after
they are received.  This function returns the old timeout value.  If
you pass in no value then it will return the old value without
modifying the current value.  The default value is 10 seconds.

=item server_fault(STRING, STRING)

Create a B<RPC::XML::fault> object of the specified type, optionally including
the second (string) parameter. See L</"Server Faults"> for the list of faults
defined by B<RPC::XML::Server> (as well as documentation on creating your
own).

=item add_method(FILE | HASHREF | OBJECT)

=item add_procedure(FILE | HASHREF | OBJECT)

=item add_function(FILE | HASHREF | OBJECT)

This adds a new published method/procedure/function to the server object that
invokes it. The new method may be specified in one of three ways: as a
filename, a hash reference or an existing object (generally of either
B<RPC::XML::Procedure>, B<RPC::XML::Method> or B<RPC::XML::Function> classes).

If passed as a hash reference, the following keys are expected:

=over 4

=item B<name>

The published (externally-visible) name for the method.

=item B<version>

An optional version stamp. Not used internally, kept mainly for informative
purposes.

=item B<hidden>

If passed and evaluates to a C<true> value, then the method should be hidden
from any introspection API implementations. This parameter is optional, the
default behavior being to make the method publically-visible.

=item B<code>

A code reference to the actual Perl subroutine that handles this method. A
symbolic reference is not accepted. The value can be passed either as a
reference to an existing routine, or possibly as a closure. See L</"How
Procedures are Called"> for the semantics the referenced subroutine must
follow.

=item B<signature>

A list reference of the signatures by which this routine may be invoked. Every
method has at least one signature. Though less efficient for cases of exactly
one signature, a list reference is always used for sake of consistency.

=item B<help>

Optional documentation text for the method. This is the text that would be
returned, for example, by a B<system.methodHelp> call (providing the server
has such an externally-visible method).

=back

If a file is passed, then it is expected to be in the XML-based format,
described in the B<RPC::XML::Procedure> page (see
L<RPC::XML::Procedure|RPC::XML::Procedure>).  If the name passed is not an
absolute pathname, then the file will be searched for in any directories
specified when the object was instantiated, then in the directory into which
this module was installed, and finally in the current working directory. If the
operation fails, the return value will be a non-reference, an error
message. Otherwise, the return value is the object reference.

The B<add_method>, B<add_function> and B<add_procedure> calls are essentialy
identical unless called with hash references. Both files and objects contain
the information that defines the type (method vs. procedure) of the
funtionality to be added to the server. If B<add_method> is called with a file
that describes a procedure, the resulting addition to the server object will be
a B<RPC::XML::Procedure> object, not a method object.

For more on the creation and manipulation of procedures and methods as
objects, see L<RPC::XML::Procedure|RPC::XML::Procedure>.

=item delete_method(NAME)

=item delete_procedure(NAME)

=item delete_function(NAME)

Delete the named method/procedure/function from the calling object. Removes the
entry from the internal table that the object maintains. If the method is
shared across more than one server object (see L</share_methods>), then the
underlying object for it will only be destroyed when the last server object
releases it. On error (such as no method by that name known), an error string
is returned.

The B<delete_procedure> and B<delete_function> calls are identical, supplied
for the sake of symmetry. All calls return the matched object regardless of its
underlying type.

=item list_methods

=item list_procedures

=item list_functions

This returns a list of the names of methods and procedures the server current
has published.  Note that the returned values are not the method objects, but
rather the names by which they are externally known. The "hidden" status of a
method is not consulted when this list is created; all methods and procedures
known are listed. The list is not sorted in any specific order.

The B<list_procedures> and B<list_functions> calls are provided for
symmetry. All calls list all published routines on the calling server object,
regardless of underlying type.

=item xpl_path([LISTREF])

Get and/or set the object-specific search path for C<*.xpl> files (files that
specify methods) that are specified in calls to B<add_method>, above. If a
list reference is passed, it is installed as the new path (each element of the
list being one directory name to search). Regardless of argument, the current
path is returned as a list reference. When a file is passed to B<add_method>,
the elements of this path are searched first, in order, before the
installation directory or the current working directory are searched.

=item get_method(NAME)

=item get_procedure(NAME)

=item get_function(NAME)

Returns a reference to an object of the class B<RPC::XML::Method>,
B<RPC::XML::Function> or B<RPC::XML::Procedure>, which is the current binding
for the published method NAME. If there is no such method known to the server,
then C<undef> is returned. Note that this is a referent to the object as stored
on the server object itself, and thus changes to it could affect the behavior
of the server.

The B<get_procedure> and B<get_function> calls are provided for symmetry. All
will return the same object for NAME, regardless of the underlying type.

=item server_loop(HASH)

Enters the connection-accept loop, which generally does not return. This is
the C<accept()>-based loop of B<HTTP::Daemon> if the object was created with
an instance of that class as a part. Otherwise, this enters the run-loop of
the B<Net::Server> class. It listens for requests, and marshalls them out via
the C<dispatch> method described below. It answers HTTP-HEAD requests
immediately (without counting them on the server statistics) and efficiently
by using a cached B<HTTP::Response> object.

Because infinite loops requiring a C<HUP> or C<KILL> signal to terminate are
generally in poor taste, the B<HTTP::Daemon> side of this sets up a localized
signal handler which causes an exit when triggered. By default, this is
attached to the C<INT> signal. If the B<Net::Server> module is being used
instead, it provides its own signal management.

The arguments, if passed, are interpreted as a hash of key/value options (not
a hash reference, please note). For B<HTTP::Daemon>, only one is recognized:

=over 4

=item B<signal>

If passed, should be the traditional name for the signal that should be bound
to the exit function. If desired, a reference to an array of signal names may
be passed, in which case all signals will be given the same handler. The user
is responsible for not passing the name of a non-existent signal, or one that
cannot be caught. If the value of this argument is 0 (a C<false> value) or the
string C<NONE>, then the signal handler will I<not> be installed, and the
loop may only be broken out of by killing the running process (unless other
arrangements are made within the application).

=back

The options that B<Net::Server> responds to are detailed in the manual pages
for that package. All options passed to C<server_loop> in this situation are
passed unaltered to the C<run()> method in B<Net::Server>.

=item dispatch(REQUEST)

This is the server method that actually manages the marshalling of an incoming
request into an invocation of a Perl subroutine. The parameter passed in may
be one of: a scalar containing the full XML text of the request, a scalar
reference to such a string, or a pre-constructed B<RPC::XML::request> object.
Unless an object is passed, the text is parsed with any errors triggering an
early exit. Once the object representation of the request is on hand, the
parameter data is extracted, as is the method name itself. The call is sent
along to the appropriate subroutine, and the results are collated into an
object of the B<RPC::XML::response> class, which is returned. Any non-reference
return value should be presumed to be an error string.

The dispatched method may communicate error in several ways.  First, any
non-reference return value is presumed to be an error string, and is encoded
and returned as an B<RPC::XML::fault> response.  The method is run under an
C<eval()>, so errors conveyed by C<$@> are similarly encoded and returned.  As
a special case, a method may explicitly C<die()> with a fault response, which
is passed on unmodified.

=item add_default_methods([DETAILS])

This method adds all the default methods (those that are shipped with this
extension) to the calling server object. The files are denoted by their
C<*.xpl> extension, and are installed into the same directory as this
B<Server.pm> file. The set of default methods are described below (see
L<"The Default Methods Provided">).

If any names are passed as a list of arguments to this call, then only those
methods specified are actually loaded. If the C<*.xpl> extension is absent on
any of these names, then it is silently added for testing purposes. Note that
the methods shipped with this package have file names without the leading
C<status.> part of the method name. If the very first element of the list of
arguments is C<except> (or C<-except>), then the rest of the list is
treated as a set of names to I<not> load, while all others do get read. The
B<Apache::RPC::Server> module uses this to prevent the loading of the default
C<system.status> method while still loading all the rest of the defaults. (It
then provides a more Apache-centric status method.)

Note that there are no symmetric calls in this case. The provided API is
implemented as methods, and thus only this interface is provided.

=item add_methods_in_dir(DIR [, DETAILS])

=item add_procedures_in_dir(DIR [, DETAILS])

=item add_functions_in_dir(DIR [, DETAILS])

This is exactly like B<add_default_methods> above, save that the caller
specifies which directory to scan for C<*.xpl> files. In fact, the
B<add_default_methods> routine simply calls this routine with the installation
directory as the first argument. The definition of the additional arguments is
the same as above.

B<add_procedures_in_dir> and B<add_functions_in_dir> are provided for symmetry.

=item share_methods(SERVER, NAMES)

=item share_procedures(SERVER, NAMES)

=item share_functions(SERVER, NAMES)

The calling server object shares the methods/procedures/functions listed in
B<NAMES> with the source-server passed as the first object. The source must
derive from this package in order for this operation to be permitted. At least
one method must be specified, and all are specified by name (not by object
reference). Both objects will reference the same exact B<RPC::XML::Procedure>
(or derivative thereof) object in this case, meaning that call-statistics and
the like will reflect the combined data. If one or more of the passed names are
not present on the source server, an error message is returned and none are
copied to the calling object.

Alternately, one or more of the name parameters passed to this call may be
regular-expression objects (the result of the B<qr> operator). Any of these
detected are applied against the list of all available methods known to the
source server. All matching ones are inserted into the list (the list is pared
for redundancies in any case). This allows for easier addition of whole
classes such as those in the C<system.*> name space (via C<qr/^system[.]/>),
for example. There is no substring matching provided. Names listed in the
parameters to this routine must be either complete strings or regular
expressions.

The B<share_procedures> and B<share_functions> calls are provided for symmetry.

=item copy_methods(SERVER, NAMES)

=item copy_procedures(SERVER, NAMES)

=item copy_functions(SERVER, NAMES)

These behave like the methods B<share_*> above, with the exception that
the calling object is given a clone of each method, rather than referencing
the same exact method as the source server. The code reference part of the
method is shared between the two, but all other data are copied (including a
fresh copy of any list references used) into a completely new
B<RPC::XML::Procedure> (or derivative) object, using the C<clone()> method
from that class. Thus, while the calling object has the same methods
available, and is re-using existing code in the Perl runtime, the method
objects (and hence the statistics and such) are kept separate. As with the
above, an error is flagged if one or more are not found.

This routine also accepts regular-expression objects with the same behavior and
limitations. Again, B<copy_procedures> and B<copy_functions> are provided for
symmetry.

=back

=head2 Specifying Server-Side Remote Procedures

Specifying the methods themselves can be a tricky undertaking. Some packages
(in other languages) delegate a specific class to handling incoming requests.
This works well, but it can lead to routines not intended for public
availability to in fact be available. There are also issues around the access
that the methods would then have to other resources within the same running
system.

The approach taken by B<RPC::XML::Server> (and the B<Apache::RPC::Server>
subclass of it) require that remote procedures be explicitly published in one
of the several ways provided. Procedures may be added directly within code by
using B<add_procedure>/B<add_method>/B<add_function> as described above, with
full data provided for the code reference, signature list, etc. The
B<add_*> technique can also be used with a file that conforms to a
specific XML-based format (detailed in the manual page for the
B<RPC::XML::Procedure> class, see L<RPC::XML::Procedure|RPC::XML::Procedure>).
Entire directories of files may be added using B<add_methods_in_dir>, which
merely reads the given directory for files that appear to be method
definitions.

=head2 The Three Types of Procedures

There are three types of procedures that B<RPC::XML::Server> marshalls calls
to. All are provided by the B<RPC::XML::Procedure> module. You should not
need to load or reference this module directly, as loading B<RPC::XML::Server>
(or a derivative) makes it available. The three types are:

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

=head2 How Procedures Are Called

When a routine is called via the server dispatcher, it is called with the
arguments that the client request passed. Depending on whether the routine is
considered a "function", a "procedure" or a "method", there may be an extra
argument at the head of the list. The extra argument is present when the
routine being dispatched is part of a B<RPC::XML::Method> object. The extra
argument is a reference to a B<RPC::XML::Server> object (or a subclass
thereof). This is derived from a hash reference, and will include these special
keys:

=over 4

=item method_name

This is the name by which the method was called in the client. Most of the
time, this will probably be consistent for all calls to the server-side
method. But it does not have to be, hence the passing of the value.

=item signature

This is the signature that was used, when dispatching. Perl has a liberal
view of lists and scalars, so it is not always clear what arguments the client
specifically has in mind when calling the method. The signature is an array
reference containing one or more datatypes, each a simple string. The first
of the datatypes specifies the expected return type. The remainder (if any)
refer to the arguments themselves.

=item peerfamily

This is the address family, C<AF_INET> or C<AF_INET6>, of a network address of
the client that has connected and made the current request. It is required
for unpacking C<peeraddr> properly.

=item peeraddr

This is the address part of a packed B<SOCKADDR_IN> or B<SOCKADDR_IN6>
structure, as returned by L<Socket/pack_sockaddr_in> or
L<Socket/pack_sockaddr_in6>, which contains the address of the client that has
connected and made the current request. This is provided "raw" in case you
need it. While you could re-create it from C<peerhost>, it is readily
available in both this server environment and the B<Apache::RPC::Server>
environment and thus included for convenience. Apply L<Socket/inet_ntop> to
C<peerfamily> and this value to obtain textual representation of the address.

=item peerhost

This is the address of the remote (client) end of the socket, in C<x.x.x.x>
(dotted-quad) format. If you wish to look up the clients host-name, you
can use this to do so or utilize the encoded structure above directly.

=item peerport

This is the port of the remote (client) end of the socket, taken from the
B<SOCKADDR_IN> structure.

=item request

The L<HTTP::Request|HTTP::Request> object for this request. Can be used to read
HTTP headers sent by the client (C<X-Forwarded-For> for your access checks, for
example).

=back

Those keys should only be referenced within method code itself, as they are
not set on the server object outside of that context.

Note that by passing the server object reference first, method-classed
routines are essentially expected to behave as actual methods of the server
class, as opposed to ordinary functions. Of course, they can also discard the
initial argument completely.

The routines should not make (excessive) use of global variables, for obvious
reasons. When the routines are loaded from XPL files, the code is created as a
closure that forces execution in the B<RPC::XML::Procedure> package (unless the
XPL specifies a namespace, see L<RPC::XML::Procedure|RPC::XML::Procedure>). If
the code element of a procedure/method is passed in as a direct code reference
by one of the other syntaxes allowed by the constructor, the package may well
be different. Thus, routines should strive to be as localized as possible,
independent of specific namespaces. If a group of routines are expected to work
in close concert, each should explicitly set the namespace with a C<package>
declaration as the first statement within the routines themselves.

=head2 The Default Methods Provided

The following methods are provided with this package, and are the ones
installed on newly-created server objects unless told not to. These are
identified by their published names, as they are compiled internally as
anonymous subroutines and thus cannot be called directly:

=over 4

=item B<system.identity>

Returns a B<string> value identifying the server name, version, and possibly a
capability level. Takes no arguments.

=item B<system.introspection>

Returns a series of B<struct> objects that give overview documentation of one
or more of the published methods. It may be called with a B<string>
identifying a single routine, in which case the return value is a
B<struct>. It may be called with an B<array> of B<string> values, in which
case an B<array> of B<struct> values, one per element in, is returned. Lastly,
it may be called with no input parameters, in which case all published
routines are documented.  Note that routines may be configured to be hidden
from such introspection queries.

=item B<system.listMethods>

Returns a list of the published methods or a subset of them as an B<array> of
B<string> values. If called with no parameters, returns all (non-hidden)
method names. If called with a single B<string> pattern, returns only those
names that contain the string as a substring of their name (case-sensitive,
and this is I<not> a regular expression evaluation).

=item B<system.methodHelp>

Takes either a single method name as a B<string>, or a series of them as an
B<array> of B<string>. The return value is the help text for the method, as
either a B<string> or B<array> of B<string> value. If the method(s) have no
help text, the string will be null.

=item B<system.methodSignature>

As above, but returns the signatures that the method accepts, as B<array> of
B<string> representations. If only one method is requests via a B<string>
parameter, then the return value is the corresponding array. If the parameter
in is an B<array>, then the returned value will be an B<array> of B<array> of
B<string>.

=item B<system.multicall>

This is a simple implementation of composite function calls in a single
request. It takes an B<array> of B<struct> values. Each B<struct> has at least
a C<methodName> member, which provides the name of the method to call. If
there is also a C<params> member, it refers to an B<array> of the parameters
that should be passed to the call.

=item B<system.status>

Takes no arguments and returns a B<struct> containing a number of system
status values including (but not limited to) the current time on the server,
the time the server was started (both of these are returned in both ISO 8601
and UNIX-style integer formats), number of requests dispatched, and some
identifying information (hostname, port, etc.).

=back

In addition, each of these has an accompanying help file in the C<methods>
sub-directory of the distribution.

These methods are installed as C<*.xpl> files, which are generated from files
in the C<methods> directory of the distribution using the B<make_method> tool
(see L<make_method|make_method>). The files there provide the Perl code that
implements these, their help files and other information.

=head2 Content Compression

The B<RPC::XML::Server> class now supports compressed messages, both incoming
and outgoing. If a client indicates that it can understand compressed content,
the server will use the B<Compress::Zlib> (available from CPAN) module, if
available, to compress any outgoing messages above a certain threshold in
size (the default threshold is set to 4096 bytes). The following methods are
all related to the compression support within the server class:

=over 4

=item compress

Returns a false value if compression is not available to the server object.
This is based on the availability of the B<Compress::Zlib> module at start-up
time, and cannot be changed.

=item compress_thresh([MIN_LIMIT])

Return or set the compression threshold value. Messages smaller than this
size in bytes will not be compressed, even when compression is available, to
save on CPU resources. If a value is passed, it becomes the new limit and the
old value is returned.

=back

=head2 Spooling Large Messages

If the server anticipates handling large out-bound messages (for example, if
the hosted code returns large Base64 values pre-encoded from file handles),
the C<message_file_thresh> and C<message_temp_dir> settings may be used in a
manner similar to B<RPC::XML::Client>. Specifically, the threshold is used to
determine when a message should be spooled to a filehandle rather than made
into an in-memory string (the B<RPC::XML::base64> type can use a filehandle,
thus eliminating the need for the data to ever be completely in memory). An
anonymous temporary file is used for these operations.

Note that the message size is checked before compression is applied, since the
size of the compressed output cannot be known until the full message is
examined. It is possible that a message will be spooled even if its compressed
size is below the threshold, if the uncompressed size exceeds the threshold.

=over 4

=item message_file_thresh

=item message_temp_dir

These methods may be used to retrieve or alter the values of the given keys
as defined earlier for the C<new> method.

=back

=head2 Server Faults

Previous versions of this library had a very loosely-organized set of fault
codes that a server might return in certain (non-fatal) error circumstances.
This has been replaced by a more configurable, adjustable system to allow
users to better integrate the server-defined faults with any that their
application may produce. It also allows for the definition of additional
fault types so that the same mechanism for formatting the pre-defined faults
can be used within sub-classes and user applications.

The server method B<server_fault> is used to generate B<RPC::XML::fault>
objects for these situations. It takes one or two arguments, the first being
the name of the type of fault to create and the second being the specific
message. If a fault is defined with a static message, the second argument may
be skipped (and will be ignored if passed).

In addition to defining their own faults, a user may override the definition
of any of the server's pre-defined faults.

=head3 Defining faults

The user may define their own faults using the C<fault_table> argument to the
constructor of the server class being instantiated. They may also override
any of the pre-defined faults (detailed in the next section) by providing a
new definition for the name.

The value of the C<fault_table> argument is a hash reference whose keys are
the names of the faults and whose values are one of two types:

=over 4

=item An integer

If the value for the key is a scalar, it is assumed to be an integer and will
be used as the fault code. When the fault is created, the message argument
(the second parameter) will be used verbatim as the fault message.

=item A 2-element list reference

If the value is a list reference, it is assumed to have two elements: the first
is the integer fault code to use, and the second is a message "template"
string to use as the fault message. If the string contains the sequence C<%s>,
this will be replaced with the message argument (the second parameter) passed
to B<server_fault>. If that sequence is not in the string, then the fault
message is considered static and the message argument is ignored.

=back

An example of defining faults:

    my $server = RPC::XML::Server->new(
        ...
        fault_table => {
            limitexceeded => [ 500 => 'Call limit exceeded' ],
            accessdenied  => [ 600 => 'Access denied: %s' ],
            serviceclosed => 700
        },
        ...
    );

In this example, the fault-type "limitexceeded" is defined as having a fault
code of 500 and a static message of C<Call limit exceeded>. The next fault
defined is "accessdenied", which has a code of 600 and message that starts
with C<Access denied:> and incorporates whatever message was passed in to the
fault creation. The last example defines a fault called C<serviceclosed> that
has a code of 700 and uses any passed-in message unaltered.

=head3 Server-defined faults

The B<RPC::XML::Server> class defines the following faults and uses them
internally. You can override the codes and messages for these by including them
in the table passed as a C<fault_table> argument. The faults fall into three
groups:

=over 4

=item Request Initialization

Faults in this group stem from the initialization of the request and the
parsing of the XML. The codes for this group fall in the range 100-199.

=item Method Resolution

This group covers problems with mapping the request to a known method or
function on the server. These codes will be in the range 200-299.

=item Execution

Lastly, these faults are for problems in actually executing the requested
code. Their codes are in the range 300-399.

=back

The faults, and the phases they apply to, are:

=over 4

=item badxml (Request Initialization)

This fault is sent back to the client when the XML of the request did not
parse as a valid XML-RPC request.

The code is C<100>, and the message is of the form, C<XML parse error: %s>.
The specific error from the XML parser is included in the message.

=item badmethod (Method Resolution)

This fault is sent when the requested method is unknown to the server. No
method has been configured on the server by that name.

The code is C<200>, and the message is of the form, C<Method lookup error: %s>.
The name of the method and other information is included in the message.

=item badsignature (Method Resolution)

If a method is known on the server, but there is no signature that matches the
sequence of arguments passed, this fault is returned. This fault cannot be
triggered by server-side code configured via B<RPC::XML::Function>, as no
signature-checking is done for those.

The code is C<201>, and the message is of the form, C<Method signature error:
%s>. The name of the method and the signature of the arguments is included in
the message.

=item execerror (Execution)

This fault relates back to the client any exception thrown by the remote code
during execution. If the invoked code returned their error in the form of a
B<RPC::XML::fault> object, that fault is returned instead. Otherwise, the
value of C<$@> is used in the message of the fault that gets generated.

The code is C<300>, and the message is of the form, C<Code execution error:
%s>. The actual text of the exception thrown is included in the message.

=back

There is one special server-fault whose code and message cannot be overridden.
If a call is made to B<server_fault> for an unknown type of fault, the
returned object will have a code of C<-1> and a message stating that the
fault-type is unknown. The message will include both the requested type-name
and any message (if any) that was passed in.

=head3 Adjusting the server-defined codes

If you just want to "move" the range of codes that the server uses out of the
way of your application's own faults, this can be done with the
C<fault_code_base> parameter when constructing the server object. The value
of the parameter must be an integer, and it is added to the value of all
existing fault codes. For example, a value of C<10000> would make the code
for the C<badxml> fault be C<10100>, the code for C<badmethod> be C<10200>,
etc.

This is applied before any user-defined faults are merged in, so their code
values will not be affected by this value.

=head1 DIAGNOSTICS

Unless explicitly stated otherwise, all methods return some type of reference
on success, or an error string on failure. Non-reference return values should
always be interpreted as errors unless otherwise noted.

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

L<RPC::XML|RPC::XML>, L<RPC::XML::Client|RPC::XML::Client>,
L<RPC::XML::ParserFactory|RPC::XML::ParserFactory>

=head1 AUTHOR

Randy J. Ray C<< <rjray@blackperl.com> >>

=cut
