# Nothing exciting, just a couple of utility routines that are used in several
# test suites

use IO::Socket;
use Socket ();
use Carp ();

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
    my ($pid, $no_sleep) = @_;

    # Per RT 27778, use 'KILL' instead of 'INT' as the stop-server signal for
    # MSWin platforms:
    my $SIGNAL = ($^O eq 'MSWin32') ? 'KILL' : 'INT';
    kill $SIGNAL, $pid;
    if (! $no_sleep)
    {
        sleep 2; # give the old sockets time to go away
    }

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

sub pack_sockaddr_any
{
    my ($family, $address, $port) = @_;

    my $packed_address = Socket::inet_pton($family, $address);
    my $packet;
    if ($family == Socket::AF_INET) {
        $packet = Socket::pack_sockaddr_in($port, $packed_address);
    } elsif ($family == Socket::AF_INET6) {
        $packet = Socket::pack_sockaddr_in6($port, $packed_address);
    } else {
        Carp::croak "Unsupported address family: $family";
    }
    return $packet;
}

sub resolve {
    my ($family, $hostname) = @_;

    my ($error, @res) = Socket::getaddrinfo($hostname, '',
        { socktype => Socket::SOCK_STREAM });
    if ($error) {
        Carp::croak "Could not resolve $hostname: $error";
    }
    my @addresses;
    while (my $ai = shift @res) {
        my ($error, $address) = Socket::getnameinfo($ai->{addr},
            Socket::NI_NUMERICHOST, Socket::NIx_NOSERV);
        push @addresses, $address;
    }
    return @addresses;
}

1;
