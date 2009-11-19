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
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use TSG;

sub new {
  my $class = shift;
  my %params = @_;
  my $self = { 
    iters => 100,
    stop => 1.0 - 1e-5,
    rundir => $ENV{PWD},
    verbosity => 1,
    alpha => 1,        # default alpha
  };

  map { $self->{$_} = $params{$_} } keys %params;
  bless($self,$class);

  return $self;
}

sub corpus {
  my ($self,$corpus) = @_;
  $self->{corpus} = $corpus;
}

sub count {
  die "* FATAL: didn't override count()!";
}

my $debug = 0;
my $loghandle;

# visits each node in the corpus and makes random sampling decisions
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

# decrements the value of a key in a hash, deleting the key if the
# count reaches 0
sub decrement {
  my ($hash,$key) = @_;
  if (exists $hash->{$key}) {
    $hash->{$key}--;
    delete $hash->{$key} if (0 == $hash->{$key});
  }
}

sub rand_transition {
  my $prob = shift;
  return (rand() < $prob) ? 1 : 0;
}

sub compress_files {
  map { system("bzip2 -9 $_") } @_;
}

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

sub dump_counts {
  print "* WARNING: dump_counts() not implemented!\n";
}

sub read_base_grammar {
  my ($self,$file) = @_;
  open RULES, $file or die "can't read base grammar event probs file '$file'";
  while (my $line = <RULES>) {
    chomp($line);
    my ($label,$stop_prob,$num_rhs,%rhs) = split(' ',$line);
    $self->{stops}{$label}  = $stop_prob;
    $self->{pairs}{$label} = \%rhs;
  }
  close RULES;
}

# returns a random element from the array in proportion to the value
# of those elements
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

