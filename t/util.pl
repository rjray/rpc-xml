# Nothing exciting, just a couple of utility routines that are used in several
# test suites

use IO::Socket;

sub start_server
{
    my $S = shift;

    my $pid;

    if (! defined($pid = fork()))
    {
        die "fork() error: $!, stopped";
    }
    elsif ($pid)
    {
        return $pid;
    }
    else
    {
        $S->server_loop(@_);
        exit; # When the parent stops this server, we want to stop this child
    }
}

sub find_port
{
    my $start_at = $_[0] || 9000;

    my ($port, $sock);

    for ($port = $start_at; $port < ($start_at + 1000); $port++)
    {
        $sock = IO::Socket->new(Domain   => AF_INET,
                                PeerAddr => 'localhost',
                                PeerPort => $port);
        return $port unless ref $sock;
    }

    -1;
}

1;
