#!/usr/bin/perl
# $Id: server.pl,v 1.4 2008/06/16 11:24:09 post Exp $

use IO::Socket;
use POSIX qw/strftime/;

if (@ARGV < 3) {
  print "Usage: server.pl <configfile> <port> <chunk size>\n";
  exit;
}

my $corpus = $ARGV[0];
$server_port = $ARGV[1];
my $chunk_size = $ARGV[2];
my $verb = $ARGV[3];

chomp(my $rundir = `pwd`);
my $runid = "parse";

my $corpus_size = 0;
open READ, $corpus or die "can't open $corpus";
while (<READ>) {
    $corpus_size++;
}
close READ;

print "Will distribute $corpus_size sentences from $corpus\n" if $verb;

$server = IO::Socket::INET->new(LocalPort => $server_port,
                                Type      => SOCK_STREAM,
                                Reuse     => 1,
                                Listen    => 10 )   # or SOMAXCONN
    or die "Couldn't be a tcp server on port $server_port : $@\n";

my $sentno = 1;
while ($client = $server->accept( )) {

  my $client_id;
  $client->recv($client_id, 100);
  chomp $client_id;

  my( $start, $stop, $file );
  $time = 1;
  do {
    if ($time > 1) {
      print "* Skipped ($start $stop) as it was already completed\n";
      $sentno = $stop + 1;
    }
    $start = $sentno;
    $stop = min($corpus_size, $sentno + $chunk_size - 1);
    $file = "${rundir}/out.${runid}.${start}-${stop}";
    $time++;
  } while (-e $file);

  my $msg = "";
  if ($sentno > $corpus_size) {
    $msg = "0";
  } else {
    $msg = "$start $stop";
  }

  $time = strftime "%F %T", localtime();
  $jobs{$client_id} = $time;  # record the job

  $client_name = getname($client);
  print "[$time] Sending ($msg) to $client_name\n" if $verb;
  $client->send("$msg\n");
  $client->close();

  # mark the client as being done
  delete $jobs{$client_id} if ($msg eq "0");

  # quit if there are no more clients
  last unless scalar keys %jobs;

  $sentno = $stop + 1;
}

close($server);


sub getname
{
  $client = shift;
  $other_end        = getpeername($client)
      or die "Couldn't identify other end: $!\n";
  my ($port, $iaddr)   = unpack_sockaddr_in($other_end);
#   $actual_ip        = inet_ntoa($iaddr);
  $claimed_hostname = gethostbyaddr($iaddr, AF_INET);

  $claimed_hostname;
}


sub min
{
  my ($a,$b) = @_;

  if ($a < $b) {
    return $a;
  }

  return $b;
}

sub read_param {
  my $param = shift;

  open READ, $configfile || die "can't read $configfile";
  while (my $line = <READ>) {
    if ($line =~ /^\s*$param\b/) {
      chomp $line;
      my $value = (split /=/, $line)[1];
      $value =~ s/^\s*|\s*$//g;
      return $value;
    }
  }
  close READ;
  return "";
}
