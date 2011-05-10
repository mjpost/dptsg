#!/usr/bin/perl
# Matt Post <post@cs.rochester.edu>

# Gibbs sampler for learning a tree substitution grammar with a
# Dirichlet process prior.  See 
#
# "Bayesian learning of a tree substitution grammar". Matt Post and
# Daniel Gildea. ACL (short paper). Singapore.
#
# for more information.
#
# Sample usage:
# tsg.pl -alpha 100 -stop 0.9 -lexicon lex -thresh 0 -corpus corpus -pcfg pcfg
#
# where 
# - {alpha} and {stop} are hyperparameters to the DP prior
# - {lexicon} determines the lexicon used to convert leaves to UNK tokens
# - all words occuring fewer than {thresh} times (from the lexicon)
#   are converted to an unknown word category
# - {corpus} is the WSJ corpus, one parse tree per line, in parenthetical
#   form, with the root node being TOP
# - {pcfg} is the MLE depth-one PCFG grammar used in the base measure
# 
# Arguments can be passed as environment variables or command-line
# arguments, with the latter overriding the former (and both
# overriding code-supplied defaults).

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use POSIX qw|strftime|;
use List::Util qw|reduce min max|;
use Memoize;
use Sampler qw(compress_files);
use Sampler::TSG;
use TSG;
#use Clone qw/clone/;

# parameters (via environment variables and command-line params)
my %PARAMS = (
  alpha => 10,    # DP parameter
  iters => 500,   # number of iterations
  stop => 0.9,    # stop prob for base geometric distribution
  log => 0,   
  lexicon => "$basedir/data/lex.02-21",
  pcfg => "$basedir/data/pcfg_rules.prb",
  '*unordered' => 0,  # whether RHS of PCFG should be considered unordered
  thresh => 1,     # threshold for converting words to UNKs
  corpus => "$basedir/data/wsj.trees.02-21.clean",
  rundir => $ENV{PWD},
  '*two' => 0,  # sample two nodes every {two}th iter (empty arg = 1)
  dump => 1,      # frequency with which to dump corpus and counts
  '*startover' => 0,   # start over even if there are existing iters completed
  srand => undef,
  verbosity => 1 );

# process command-line parameters
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
# base model, so read them in to pass them into the sampler
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
print STDERR "done (read " . (scalar keys %rules) . " rules).\n";

$PARAMS{rules} = \%rules;

# find the highest directory from a previous run, and pick up from
# there unless -startover was specified on the command line
my $PICKING_UP = 0;
my $bzip = "/usr/bin/bzip2";
$bzip = "$ENV{HOME}/bin/bzip2" if ! -e $bzip;

opendir DIR, $PARAMS{rundir} or die "can't read files in rundir '$PARAMS{rundir}'";
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

    if ( (scalar @{$tree->{children}}) > 1 ) {
      my $found = ruleof($tree,1);
      print "\n* FATAL: tree $.: top-level rule must be unary rule labeled 'TOP'\n  (found '$found')\n";
      exit(1);
    }

    push(@corpus, $tree)
        if defined $tree;
  }
  close CORPUS;
  print STDERR "done.\n";
} else {

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
    push(@corpus, $tree)
        if defined $tree;
  }
  close CORPUS;
  if (-e $corpus and ! -e "${corpus}.bz2") {
    compress_files($corpus);
  }
  print STDERR "done.\n";
  $iter++;
}  

# create the sampler, passing it the command line parameters (some of
# which it might use), and set the corpus, which has been read in.
my $sampler = new Sampler::TSG(%PARAMS);
$sampler->corpus(\@corpus);
$sampler->count();

# print "saw $size events\n";

# iterate until completion ($iter was set earlier, in case we picked
# up from an existing run)
for ( ; $iter <= $PARAMS{iters}; $iter++) {
  print "ITERATION $iter TIMESTAMP ", , $/;

  # allows for easy kills when you don't know which process is running
  # in the current dir and only want to kill that one
  my $stop_file = ".stop";
  if (-e $stop_file) {
    unlink($stop_file);
    print "QUITTING ON PRESENCE OF STOP FILE\n";
    exit;
  }

  # log
  open $sampler->{logfh}, ">log.$iter" if ($PARAMS{log});

  my $start_time = time;

  # sample_all visits all nodes of all sentences in the corpus, and
  # applies the function pointer passed to it to each of those nodes.
  # Here, we pass it a function that considers either one or two nodes
  # at a time, depending on the program arguments
  if ($PARAMS{two} && !($iter % $PARAMS{two})) {
    $sampler->sample_all($iter,$sampler->can('sample_two'));
  } else {
    $sampler->sample_all($iter,$sampler->can('sample_each_TSG'));
  }

  # sanity check -- put in place after a bug was discovered (we cache
  # the LHS counts, and were not incrementing/decrementing them
  # correctly)
  if ($sampler->check_counts()) {
    print "ITERATION $iter passed sanity check.\n";
  } else {
    print "* FATAL: failed sanity check.\n";
    exit;
  }

#   map { print "$counts{$_} '$_'\n" } (keys %counts);
  my $dur = time() - $start_time;
  my $nicedur = mytime($dur);
  mylog("ITERATION $iter took $dur seconds ($nicedur)",1);

  my $types = $sampler->types();
  my $tokens = $sampler->tokens();
  mylog("ITERATION $iter splits:$sampler->{splits} merges:$sampler->{merges} types:$types tokens:$tokens",1);

  my $likelihood = $sampler->likelihood();
  mylog("ITERATION $iter log likelihood $likelihood");

#   print "ITERATION stats ", (scalar keys %counts), " keys\n";
#   my @newcorpus = map { build_tree_oneline($_) } @corpus;
#   print "ITERATION size corpus ", total_size(\@newcorpus), " counts ", total_size(\%counts), $/;

  close $sampler->{logfh} if ($PARAMS{log});

  if ($PARAMS{dump} and ! ($iter % $PARAMS{dump})) {
    $sampler->dump_corpus($iter);
    $sampler->dump_counts($iter);
  }
}

# obtains the appropriate filehandle for logging to, and logs the
# message
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
