# Nothing exciting, just a couple of utility routines that are used in several
# test suites

use IO::Socket;

sub start_server
{
    my ($S, @args) = @_;

    my $pid;

    if (! defined($pid = fork))
    {
        die "fork() error: $!, stopped";
    }
    elsif ($pid)
    {
        return $pid;
    }
    else
    {
        $S->server_loop(@args);
        exit; # When the parent stops this server, we want to stop this child
    }
}

sub stop_server
{
    my $pid = shift;

    # Per RT 27778, use 'KILL' instead of 'INT' as the stop-server signal for
    # MSWin platforms:
    my $SIGNAL = ($^O eq 'MSWin32') ? 'KILL' : 'INT';
    kill $SIGNAL, $pid;
    sleep 2; # give the old sockets time to go away

    return;
}

sub find_port
{
    my $start_at = shift;
    $start_at ||= 9000;

    for my $port ($start_at .. ($start_at + 2000))
    {
        my $sock = IO::Socket->new(
            Domain   => AF_INET,
            PeerAddr => 'localhost',
            PeerPort => $port
        );
        return $port if (! ref $sock);
    }

    return -1;
}

sub find_port_in_use
{
    my $start_at = shift;
    $start_at ||= 9000;

    for my $port ($start_at .. ($start_at + 2000))
    {
        my $sock = IO::Socket->new(
            Domain   => AF_INET,
            PeerAddr => 'localhost',
            PeerPort => $port
        );
        return $port if ref $sock;
    }

    return -1;
}

1;
