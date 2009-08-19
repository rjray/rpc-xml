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

sub stop_server
{
    my $pid = shift;

    # Per RT 27778, use 'KILL' instead of 'INT' as the stop-server signal for
    # MSWin platforms:
    my $SIGNAL = ($^O eq "MSWin32") ? 'KILL' : 'INT';
    kill $SIGNAL, $pid;
    sleep 2; # give the old sockets time to go away 
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

sub read_config
{
	my $file = shift;

	return {} unless -f $file;

	open(my $fh, "< $file") || die "Error opening $file: $!";

	my $config = {};

	while (defined($_ = <$fh>))
	{
		next if /^#/;
		chomp;
		next if /^\s*$/;

		my ($key, $value) = split(/\s*=\s*/, $_, 2);
		$value =~ s/\s+$//; # Lose trailing whitespace
		$value = [ split(/\s*,\s*/, $value) ];

		$config->{$key} = $value;
	}

	$config;
}

1;
