#!/usr/bin/perl

# Gibbs sampler for DOP with a Dirichlet process prior.

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use threads;
use threads::shared;
use POSIX qw|strftime|;
use List::Util qw|reduce min max|;
use Memoize;
use Sampler qw(compress_files);
use Sampler::TSG;
use TSG;
#use Clone qw/clone/;

# parameters (via environment variables and command-line params)
my %PARAMS = (
  alpha => 10,  # DP parameter
  iters => 500,  # number of iterations
  stop => 0.9, # stop prob for base geometric distribution
  log => 0,
  lexicon => "$basedir/data/lex.02-21",
  pcfg => "$basedir/data/pcfg_rules.prb",
  thresh => 2,
  corpus => "$basedir/data/wsj.trees.02-21.clean",
  rundir => $ENV{PWD},
  '*startover' => 0,
  srand => undef,
  verbosity => 1 );

process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

# initialize the random number generator if requested
srand($PARAMS{srand}) if defined $PARAMS{srand};

my @corpus;
my %counts;

my %rules;           # PCFG probs for regular rules
my %samples;         # samples gathered
my $size = 0;        # number of tree fragments in the corpus
my $debug = 0;
my $loghandle;

chdir $PARAMS{rundir} or die "couldn't chdir to '$PARAMS{rundir}'";

# We need the PCFG rules in order to score fragments according to the
# base model
my $pcfg_file = $PARAMS{pcfg};
print STDERR "Reading PCFG rules...";
open RULES, $pcfg_file or die "can't read PCFG rules file '$pcfg_file'";
while (my $line = <RULES>) {
  chomp($line);
  my ($prb, @tokens) = split ' ', $line;
  my $rep = join " ", @tokens;

  $rules{$rep} = $prb;
}
close RULES;
print STDERR "done.\n";

$PARAMS{rules} = \%rules;

my $PICKING_UP = 0;
my $bzip = "/usr/bin/bzip2";
$bzip = "$ENV{HOME}/bzip2" unless -e $bzip;
my $iter = 1;

# find the highest directory from a previous run
opendir DIR, $PARAMS{rundir} or die "can't read files in rundir '$rundir'";
my $iter = max(1, grep(/^\d+$/, readdir(DIR)));
closedir DIR;

if ($iter == 1 || $PARAMS{startover}) {
  $iter = 1;
  print STDERR "Initializing counts with first pass over $PARAMS{corpus}.\...";
  my $sentno = 0;
  open CORPUS, $PARAMS{corpus} or die "can't open corpus '$PARAMS{corpus}'";
  while (my $line = <CORPUS>) {
    chomp $line;
    next if $line =~ /^$/;
    $sentno++;

    my $tree = build_subtree($line,$lexicon);

    push @corpus, $tree;
  }
  close CORPUS;
  print STDERR "done.\n";
} else {
  $iter--;

  if ($iter == $PARAMS{iters}) {
    print "* Already have $iter iterations of output, quitting\n";
    exit;
  }

  $PICKING_UP = 1;
  print STDERR "Picking up where we left off (iteration $iter)...";
  my $corpus = "$iter/corpus";
  if (! -e $corpus and -e "${corpus}.bz2") {
    print "decompressing compressed corpus file ${corpus}.bz2\n";
    system("$bzip -d ${corpus}.bz2");
  }
  open CORPUS, $corpus or die "can't open corpus '$corpus'";
  while (my $line = <CORPUS>) {
    chomp $line;

    my $tree = build_subtree($line,$lexicon);
    push @corpus, $tree;
  }
  close CORPUS;
  if (-e $corpus and ! -e "${corpus}.bz2") {
    compress_files($corpus);
  }
  print STDERR "done.\n";
  $iter++;
}  

my $sampler = new Sampler::TSG(%PARAMS);
$sampler->corpus(\@corpus);
$sampler->count();

# print "saw $size events\n";

for ( ; $iter <= $PARAMS{iters}; $iter++) {
  print "ITERATION $iter TIMESTAMP ", , $/;

  # log
  open $sampler->{log}, ">log.$iter" if ($PARAMS{log});

  my $start_time = time;
  $sampler->sample_all($iter,\&sample_each_TSG);

#   map { print "$counts{$_} '$_'\n" } (keys %counts);
  my $dur = time() - $start_time;
  my $nicedur = mytime($dur);
  mylog("ITERATION $iter took $dur seconds ($nicedur)",1);
  mylog("ITERATION $iter splits:$sampler->{splits} merges:$sampler->{merges}",1);

#   print "ITERATION stats ", (scalar keys %counts), " keys\n";
#   my @newcorpus = map { build_tree_oneline($_) } @corpus;
#   print "ITERATION size corpus ", total_size(\@newcorpus), " counts ", total_size(\%counts), $/;

  close $sampler->{log} if ($PARAMS{log});

  $sampler->dump_corpus($iter);
  $sampler->dump_counts($iter);
}

sub mylog {
  my ($msg,$stdout) = @_;

  my $fh = loghandle();
  print $fh "$msg\n";
  print $msg, $/
      if $stdout;
}

sub loghandle() {
  if (! defined $loghandle) {
    if ($PICKING_UP) {
      open $loghandle, ">>out.log" or die "can't open logfile";
    } else {
      open $loghandle, ">out.log" or die "can't open logfile";
    }
  }

  return $loghandle;
}

sub mytime {
  my $secs = shift;

  my $hours = int($secs / 3600);
  $secs %= 3600;

  my $mins = int($secs / 60);
  $secs %= 60;

  my $time = "${hours}h ${mins}m ${secs}s";
  return $time;
}
