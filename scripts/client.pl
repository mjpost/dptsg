#!/usr/bin/perl
# $Id: client.pl,v 1.6 2008/09/21 16:20:10 post Exp $

use strict;
use warnings;

use IO::Socket;

my $basedir = $ENV{DPTSG} or die "Environment variable DPTSG undefined (set to code root)";
unshift @INC, $basedir;

my $id = $ENV{HOSTNAME} . "_" . $$;

my $remote_host = $ENV{server} || "mgt";
my $remote_port = $ENV{port} || 4444;
my $grammar     = $ENV{grammar} or die "no grammar";
my $corpus      = $ENV{corpus} or die "no corpus";
my $rundir      = $ENV{rundir} or die "no rundir";
my $mismatch    = $ENV{mismatch} || 0;
my $multipass   = $ENV{multipass} || "";
$multipass      = "-multipass $multipass" if $multipass;
my $esrap       = $ENV{esrap} || "";
$esrap          = "-esrap $esrap" if $esrap;
my $ruleserver  = $ENV{ruleserver} || "";
$ruleserver     = "-ruleserver $ENV{ruleserver}" if $ENV{ruleserver};
my $diffuse     = (exists $ENV{diffuse}) ? $ENV{diffuse} : 1;
my $binarize    = (exists $ENV{binarize}) ? $ENV{binarize} : "grammar";
my $collapse    = (exists $ENV{collapse}) ? $ENV{collapse} : "none";

my $answer = 1;
for (;;) {
  my $socket = IO::Socket::INET->new(PeerAddr => $remote_host,
                                  PeerPort => $remote_port,
                                  Proto    => "tcp",
                                  Type     => SOCK_STREAM)
      or die "Couldn't connect to $remote_host:$remote_port : $@\n";

# ... do something with the socket

  print $socket $id, $/;

  $answer = <$socket>;   chomp $answer;

  if ($answer eq "0" || $answer eq "") {
    last;
  }

  my ($start, $stop) = split(' ', $answer);

  my $file = "$rundir/out.parse.$start-$stop";
  my $logfile = "$rundir/log.$start-$stop";

  my $cmd = "$ENV{HOME}/code/cky/llncky -f $start -t $stop $corpus $grammar -o $file -l $logfile >> run.log.$id 2>&1";
  # print "START=$start STOP=$stop CORPUS=$corpus GRAMMAR=$grammar RUNDIR=$rundir\n";
  print $cmd . $/;
  system($cmd);

  # and terminate the connection when we're done
  close($socket);
}

print "All done.\n";

