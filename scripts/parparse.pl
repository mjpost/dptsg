#!/usr/bin/perl

use strict;
use warnings;
use threads;
use POSIX qw|ceil|;
use List::Util qw|min|;

# parallelizes parsing of a corpus

my $basedir = $ENV{DPTSG} or die "Environment variable DPTSG undefined (set to code root)";
unshift @INC, $basedir;

my $mem_est = "6000mb";
my $time_est = "4:00:00";
my $port = $ENV{port} || 7777;
my $num_clients = $ENV{clients} || 64;
my $hostname = $ENV{server} || "node64";
my $cmd = $ENV{cmd} || "$basedir/parse.pl";
my $chunk_size = $ENV{chunk_size} || 5;
my $sleep_time = (defined $ENV{sleep}) ? $ENV{sleep} : 0;
my $rundir = $ENV{rundir} || $ENV{PWD} || "/p/mt-scratch/post/dpdop/test";
my $grammar = $ENV{grammar} || "$basedir/data/pcfg_rules.prb";
my $corpus = $ENV{corpus} || "$basedir/data/wsj.23.words.max40";
my $kbest = $ENV{kbest} || 1;
my $thresh = $ENV{thresh} || 1;
my $binsize = $ENV{binsize} || 10;
my $ruleserver = $ENV{ruleserver} || "";
my $multipass = $ENV{multipass} || "";
my $esrap = $ENV{esrap} || "";
my $mismatch = $ENV{mismatch} || 0;
my $diffuse = exists $ENV{diffuse} ? $ENV{diffuse} : "max";
my $binarize = exists $ENV{binarize} ? $ENV{binarize} : "right";
my $collapse = exists $ENV{collapse} ? $ENV{collapse} : "none";

if (! $mismatch) {
  $mismatch = ($grammar =~ /\.gt$/) ? 1 : 0;
}

my $client = "$basedir/scripts/client.pl";

print "RUNDIR: $rundir\n";
print "GRAMMAR: $grammar\n";
print "CORPUS: $corpus\n";
# print "COLLAPSE: $collapse\n";

chdir $rundir || die "couldn't chdir($rundir)";

my @threads;
{
  my $thread = threads->new(sub {
      print "Starting server (port = $port, chunk_size = $chunk_size)\n";
      system("$basedir/scripts/server.pl $corpus $port $chunk_size"); });
    push @threads, $thread;
    sleep 3;
}

# start the clients
for my $num (1..$num_clients) {
    my $thread = threads->new(sub { system("qsub -l nodes=fast -l pvmem=$mem_est,walltime=$time_est,nodes=1:ppn=1 -z -v server=$hostname,port=$port,cmd=$cmd,corpus=$corpus,grammar=$grammar,binsize=$binsize,kbest=$kbest,thresh=$thresh,rundir=$rundir,ruleserver=$ruleserver,multipass=$multipass,esrap=$esrap,mismatch=$mismatch,diffuse=$diffuse,collapse=$collapse,binarize=$binarize $client") });
    push @threads, $thread;
    sleep $sleep_time;
}

foreach my $thread (@threads) {
    $thread->join();
}

unlink("out.all");
system("cat out.parse.* | sort -n | perl -pe 's/^\\d+ //' > out.all");
#system("evalb -p ~/code/dpdop/data/COLLINS.prm ~/code/dpdop/data/wsj.trees.22.clean.max40 out.all | tail | head -2");
