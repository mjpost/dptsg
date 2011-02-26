# Matt Post <post@cs.rochester.edu>

# This file contains the Sampler package, a base class defining
# features common to different Gibbs samplers on treebanks.  It is
# general enough to support two subclasses: the TSG inference
# described in Post & Gildea (2008) [Sampler/TSG.pm], and work in
# progress for inducing grammars from raw text [Sampler/Learner.pm].

package Sampler;

use strict;
use Exporter;
use vars qw|$VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS|;

# our($lexicon,%rules,%deps,%PARAMS,$base_measure);

@ISA = qw|Exporter|;
@EXPORT = qw|compress_files decrement random_multinomial|;
@EXPORT_OK = ();

use strict;
use Carp qw|croak|;
use warnings;
use threads;
use threads::shared;
use POSIX qw|strftime|;
use List::Util qw|reduce min shuffle sum|;
# use Memoize;

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

# import common subtree operations and other support functions
use TSG;

# constructor
sub new {
  my $class = shift;
  my %params = @_;
  # default values
  my $self = { 
    iters => 100,
    stop => 0.9,
    rundir => $ENV{PWD},
    verbosity => 1,
    alpha => 1,        # default alpha
  };

  map { $self->{$_} = $params{$_} } keys %params;
  bless($self,$class);

  return $self;
}

# sets the corpus (expects a list reference)
sub corpus {
  my ($self,$corpus) = @_;
  $self->{corpus} = $corpus;
}

sub count {
  die "* FATAL: didn't override count()!";
}

my $debug = 0;
my $loghandle;

# sample_all
# 
# Calls walk() on each tree in the corpus, passing along any functions
# that are given to it.  This function is the entry point for the
# Gibbs sampler.
sub sample_all {
  my ($self,$iter,@funcs) = @_;

  $self->{iter} = $iter;
  $self->{treeno} = 1;

  $| = 1;
  foreach my $tree (@{$self->{corpus}}) {
    print "ITER $iter TREE $self->{treeno}\n"
         if $self->{verbosity} and (! ($self->{treeno} % 1000));

    walk($tree, \@funcs, $self);

    $self->{treeno}++;
  }
}

# decrement
#
# Decrements the value of a key in a hash, deleting the key if the
# count reaches 0.  The deletion is done to save on memory, since the
# sampler may be creating and deleting many different keys.  This
# proves essential to running within reasonable amounts of memory (and
# not consuming the machine's entire memory due to the long tail of
# potential subtrees).
sub decrement {
  my ($hash,$key) = @_;
  if (exists $hash->{$key}) {
    $hash->{$key}--;
    delete $hash->{$key} if (0 == $hash->{$key});
  }
}

# rand_transition
#
# Make a binary decision based on a probability.  The argument is the
# probability of TRUE.
sub rand_transition {
  my $prob = shift;
  return (rand() < $prob) ? 1 : 0;
}

# compress_files
#
# Uses bzip to compress a list of files (given in @_)
sub compress_files {
  my $bzip = "/usr/bin/bzip2";
  map { system("$bzip -9 -f $_") } @_;
}

# dump_corpus
#
# Dumps the corpus of trees to disk.
sub dump_corpus {
  my ($self,$dir) = @_;
  mkdir $dir unless -d $dir;

  my @corpus = map { build_subtree_oneline($_,1) } @{$self->{corpus}};

  my $file = "$dir/corpus";
  open DUMP, ">$file" or warn "can't dump to $file";
  map { print DUMP $_, $/ } @corpus;
  close DUMP;
  compress_files($file);
}

# dump_counts
#
# Dumps the event counts to disk.  This must be defined on a
# class-by-class basis.
sub dump_counts {
  print "* WARNING: dump_counts() not implemented!\n";
}

# check_counts
#
# This is called once per iteration, at the end of the iteration, and
# allows for sanity checks.
sub check_counts {
  print "* WARNING: check_counts() not implemented\n";
}

# random_multinomial
#
# Receives a list of numbers and randomly chooses one according to
# their normalized frequencies.

sub random_multinomial {
  my ($list) = @_;

  my $len = scalar @$list;
  my $total = sum(@$list);
  my $prob = rand($total);

  my $sum = 0.0;
  my $which = 0;
  for (;;) {
    $sum += $list->[$which];
    last if $sum > $prob;
    $which++;
    last if $which >= $len;
  }

  # print "  RANDOM(" . join(",",map {$_/$total} @$list) . ") = $which\n";

  return $which;
}

sub AUTOLOAD;

# sub AUTOLOAD {
#   my ($self) = @_;
#   my $type = ref ($self) || croak "$self is not an object";
#   my $field = $AUTOLOAD;
#   $field =~ s/.*://;

#   unless (exists $self->{$field}) {
#     croak "$field does not exist in object/class $type";
#   }
#   if (@_) {
#     return $self->($name) = shift;
#   } else {
#     return $self->($name);
#   }
# }

1;

