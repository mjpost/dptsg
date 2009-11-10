package Gibbs;

use strict;
use Exporter;
use vars qw|$VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS|;

# our($lexicon,%rules,%deps,%PARAMS,$base_measure);

@ISA = qw|Exporter|;
@EXPORT = qw|compress_files base_prob base_prob_dep prob_dep|;
@EXPORT_OK = ();

use strict;
use warnings;
use threads;
use threads::shared;
use POSIX qw|strftime|;
use List::Util qw|reduce min shuffle|;
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
    beta => {},
    iters => 100,
    stop => 0.9,
    rundir => $ENV{PWD},
    verbosity => 1,
    alphas => {
      nts => 10,          # number of nonterminals
      terminals => 10,
      pairs => 10,
      rewrites => 10,
    },
    totals => {
      nts => 0,               # number of nodes in corpus
      terminals => {},        # total number of terminal rewrites for each lhs
      pairs => {},            # total number of nonterminal rewrites for each lhs
      rewrites => {},         # total number of rule rewrites for each lhs
    },
    nts => [],                # stick-breaking construction over nonterminals
    rewrites => {},           # counts of lhs -> {N U T}* (a string of terms and nts)
    pairs => {},              # counts of lhs -> N (a nonterminal)
    terminals => {},          # counts of lhs -> T (a terminal)
  };

  map { $self->{$_} = $params{$_} } keys %params;
  bless($self,$class);

#   map { print "PARAM: $_ $self->{$_}\n" } keys %$self;

  return $self;
}

sub load_deps() {
  my $self = shift;
  my $deps_file = shift || "$basedir/data/deps.02-21";
  my $FLOOR = 1e-10;

  my %deps;
  open READ, $deps_file or die "can't read deps";
  while (my $line = <READ>) {
    chomp $line;
    my ($head,$prior,$backoff,$stop,$numdeps) = (split ' ', $line);
    $deps{$head} = { __prior__   => $prior,
                     __stop__    => min($stop,1.0-$FLOOR),
                     __backoff__ => $backoff };

    for my $i (1..$numdeps) {
      chomp($line = <READ>);
      my ($dep,$prob) = split ' ', $line;
      $deps{$head}{$dep} = $prob;
    }
  }
  close READ;

  $self->{deps} = \%deps;
}

sub corpus {
  my ($self,$corpus) = @_;
  $self->{corpus} = $corpus;

  $self->{nts} = [ 0 ];
  $self->{rewrites} = {};
  $self->{pairs} = {};
  $self->{terminals} = {};
  $self->{totals}{nts} = 0;
  $self->{totals}{terminals} = {};
  $self->{totals}{pairs} = {};
  $self->{totals}{rewrites} = {};

  # functions to count events over the training corpus for initialization
  my $count = sub {
    my ($node) = @_;

    my $lhs = $node->{label};
    my $rule = ruleof($node);

    if (@{$node->{children}}) {

      $self->{nts}->[$lhs]++;
      $self->{totals}{nts}++;

      $self->{rewrites}{$lhs}{$rule}++;
      $self->{totals}{rewrites}{$lhs}++;

      foreach my $kid (@{$node->{children}}) {
        my $lab = $kid->{label};
        if (is_terminal($kid)) {
          $self->{terminals}{$lhs}{$lab}++;
          $self->{totals}{terminals}{$lhs}++;
        } else {
          $self->{pairs}{$lhs}{$lab}++;
          $self->{totals}{pairs}{$lhs}++;
        }
      }
    }
  };

  # walk over the nodes of each tree in the corpus, applying the
  # closures defined above to each of them
  map { walk($_,[$count]) } @$corpus;
}

my $debug = 0;
my $loghandle;

# visits each node in the corpus and makes random sampling decisions
sub sample_all {
  my ($self,$iter) = @_;

  $self->{iter} = $iter;
  $self->{treeno} = 1;
  $self->{deletions} = 0;
  $self->{insertions} = 0;
  $self->{renamings} = 0;

  foreach my $tree (@{$self->{corpus}}) {
    # print "ITER $iter TREE $self->{treeno}\n" if $self->{verbosity};

    walk($tree, [\&sample_each_insert], $self);
    # map { $self->sample_each_structure($_,rep($tree,$_)) } @{$tree->{children}};

    $self->{treeno}++;
  }
}

# decrements the value of a key in a hash, deleting the key if the
# count reaches 0
sub decrement {
  my ($hash,$key) = @_;
  if (exists $hash->{$key}) {
    $hash->{$key}--;
    delete $hash->{$key} unless $hash->{$key};
  }
}

sub is_internal {
  my ($node) = @_;
  return $node->{label} =~ /^\*/ ? 1 : 0;
}

sub sample_each_insert {
  my ($tree,$self) = @_;

  # print "SAMPLE_STRUCTURE($tree->{label})\n";

  # don't consider preterminal nodes
  return if is_preterminal($tree);

  my $numkids = $tree->{numkids};
  return unless $numkids;

  # INSERTIONS Randomly decide whether to place a new node over each
  # contiguous span of children.  As before, quit after succeeding,
  # and randomize the spans we choose to facilitate mixing.

  if ($tree->{numkids} > 1) {
    # print "INSERT BENEATH($tree->{label})\n";

    my $lhs = $tree->{label};

    my @span_indexes;
    for my $span (2..$tree->{numkids}-1) {
      for my $s (0..$tree->{numkids}-$span) {
        my $t = $s + $span - 1;
        push @span_indexes, [$s,$t];
      }
    }
    @span_indexes = shuffle(@span_indexes);

    for my $index (0..$#span_indexes) {
      my ($s,$t) = @{$span_indexes[$index]};

      # decrement counts
      my $current_rule = ruleof($tree);
      $self->subtract($current_rule);

      my $prob_stay = $self->prob($current_rule);

      # print "  BEFORE: ", ruleof($tree), " [$s,$t]$/";

      # create the new node and insert it over the children; the new
      # node is an internal node only if at least one of kids is an
      # internal node
      my $newnode = { label => $tree->{label} };
      my @deleted = splice(@{$tree->{children}},$s,$t-$s+1,$newnode);
      $newnode->{children} = \@deleted;

      # print "  AFTER1: ", ruleof($tree), $/;
      # print "  AFTER2: ", ruleof($newnode), $/;

      my $new_rule  = ruleof($tree);
      my $new_rule2 = ruleof($newnode);
      my $prob_insert = $self->prob($new_rule) * $self->prob($new_rule2);

      # print "$self->{treeno} INSERT $prob_insert STAY $prob_stay\n";

      my $insert_prob = ($prob_insert / ($prob_insert + $prob_stay));
      my $do_insert = rand_transition($insert_prob);

      if ($do_insert) {
        $self->{insertions}++;
        # print "DOING INSERT\n";

        $tree->{numkids} = scalar @{$tree->{children}};
        $newnode->{numkids} = scalar @{$newnode->{children}};

        map { add($_) } ($new_rule,$new_rule2);
        $self->{nts}->[$newnode->{label}]++;

        last;
      } else {
        # print "NOT DOING INSERT\n";

        # put the children back
        splice @{$tree->{children}}, $s, 1, @{$newnode->{children}};

        # restore the counts
        map { add($_) } $current_rule;

        last;
      }
    }
  }
}

sub sample_each_delete {
  my ($tree,$self) = @_;

  # print "SAMPLE_STRUCTURE($tree->{label})\n";

  # don't consider preterminal nodes
  return if is_preterminal($tree);

  my $numkids = $tree->{numkids};
  return unless $numkids;

  # DELETIONS Randomly choose whether to delete each of the
  # children, quitting when a deletion decision is made.  To
  # facilitate mixing, we consider the children in a random order.
  my @kid_indexes = shuffle(0..@{$tree->{children}}-1);
  foreach my $kidno (@kid_indexes) {
    my $kid = $tree->{children}[$kidno];
    my $numgrandkids = @{$kid->{children}};

    last unless $numgrandkids;

    # don't delete preterminals
    # actually, we may want to allow preterminals to be deleted
    # next if is_preterminal($kid);

    # print "SAMPLING WITH ($tree->{label},$kid->{label})\n";

    # There are four cases to consider, based on whether the node and
    # the child being considered are each internal or external.  Based
    # on these four possibilities, there are two possible tree
    # structure outcomes.  The following code handles all four
    # scenarios.

    my $current_rule = ruleof($tree);
    my $kid_rule     = ruleof($kid);
    $self->subtract($current_rule);
    $self->{nts}->[$kid->{label}]--;
    $self->subtract($kid_rule);
  
    # print "DELETE PROB STAY ($current_rule, $kid_rule)\n";
    my $prob_stay = $self->prob($current_rule) * $self->prob($kid_rule);
    # print "DELETE PROB STAY = ", $prob_stay, $/;

    my $deleted_node = splice @{$tree->{children}}, $kidno, 1, @{$kid->{children}};
    my $new_rule = ruleof($tree);
    my $prob_delete = $self->prob($new_rule);
    # print "DELETE PROB DELETE = ", $prob_delete, $/;
    
    my $delete_prob = ($prob_delete / ($prob_stay + $prob_delete));
    my $do_delete = rand_transition($delete_prob);

    if ($do_delete) {
      $self->{deletions}++;

      # print "  DELETING NODE $tree->{label}/[$kid->{label}]\n";

      # instead of having to update these, we should really just do
      # them through accessor functions
      $tree->{numkids} = scalar @{$tree->{children}};

      # increase the rule count
      map { add($_) } ($new_rule);

      # only delete one child at a time
      last;
    } else {
      # put the node back
      my @grandkids = splice @{$tree->{children}}, $kidno, $numgrandkids, $deleted_node;

      # restore the counts
      map { add($_) } ($current_rule, $kid_rule);
      $self->{nts}->[$kid->{label}]++;

      last;
    }
  }
}

sub sample_each_rename {
  my ($tree,$self) = @_;

  # print "SAMPLE_STRUCTURE($tree->{label})\n";

  # don't consider preterminal nodes
  return if is_preterminal($tree);

  my $numkids = $tree->{numkids};
  return unless $numkids;

  # choose a new label
  my $u = $self->random_nonterminal();

  my @kid_indexes = shuffle(0..@{$tree->{children}}-1);
  my $changed = 0;
  foreach my $kidno (@kid_indexes) {
    my $kid = $tree->{children}[$kidno];
    next if $kid->{label} eq $u;

    my $numgrandkids = @{$kid->{children}};
    last unless $numgrandkids;

    my $oldlabel = $kid->{label};

    my $current_rule1 = ruleof($tree);
    my $current_rule2 = ruleof($kid);
    $self->subtract($current_rule1);
    $self->{nts}->[$kid->{label}]--;
    $self->subtract($current_rule2);
    my $prob_stay = $self->prob($current_rule1) * $self->prob($current_rule2);

    $kid->{label} = $u;
    my $new_rule1 = ruleof($tree);
    my $new_rule2 = ruleof($kid);
    my $prob_change = $self->prob($new_rule1) * $self->prob($new_rule2);

    my $prob_rename = ($prob_change / ($prob_stay + $prob_change));
    my $do_rename = rand_transition($prob_rename);

    if ($do_rename) {
      $self->{renamings}++;

      # add in the new counts
      map { add($_) } ($new_rule1,$new_rule2);
      $self->{nts}->[$u]++;
    } else {
      # restore the old label
      $kid->{label} = $oldlabel;
      # add in the new counts
      map { add($_) } ($current_rule1,$current_rule2);
      $self->{nts}->[$oldlabel]++;
    }

    last if $changed;
  }

}

sub add {
  my ($self,$rule,$amt) = @_;

  my ($lhs,@rhs) = split(' ',$rule);
  $lhs     =~ s/^\(//;
  $rhs[-1] =~ s/\)$//;

  # rewrites
  $self->{rewrites}{$lhs}{$rule}++;
  $self->{totals}{rewrites}{$lhs}++;
  $self->{totals}{rewrites}{$lhs}++;

  foreach my $rhs (@rhs) {
    if (is_terminal($rhs)) {
      # terminals
      $self->{terminals}{$lhs}{$rhs}++;
      $self->{totals}{terminals}{$lhs}{$rhs}++;    
    } else { 
      # pairs
      $self->{pairs}{$lhs}{$rhs}++;
      $self->{totals}{pairs}{$lhs}{$rhs}++;
    }
  }
}



sub subtract {
  my ($self,$rule) = @_;
  
  print "SUBTRACT($rule)\n";
  my ($lhs,@rhs) = split(' ',$rule);
  $lhs     =~ s/^\(//;
  $rhs[-1] =~ s/\)$//;

  # rewrites
  decrement($self->{rewrites}{$lhs},$rule);
  decrement($self->{totals}{rewrites},$lhs);

  foreach my $rhs (@rhs) {
    if (islex($rhs)) {
      # terminals
      decrement($self->{terminals}{$lhs},$rhs);
      # print "TERMINALS($lhs -> $rhs) = $self->{terminals}{$lhs}{$rhs}\n";
      # print "TERMINALS[$lhs] = $self->{totals}{terminals}{$lhs}\n";
      decrement($self->{totals}{terminals},$lhs);    
    } else { 
      # pairs
      decrement($self->{pairs}{$lhs},$rhs);
      # print "PAIRS($lhs -> $rhs) = $self->{pairs}{$lhs}{$rhs}\n";
      # print "PAIRS[$lhs] = $self->{totals}{pairs}{$lhs}\n";
      decrement($self->{totals}{pairs},$lhs);
    }
  }
}

sub rand_transition {
  my $prob = shift;
  return (rand() < $prob) ? 1 : 0;
}

# Returns the top-level probability of a rule rewrite.
sub prob {
  my ($self,$rule,$verb) = @_;

  my $lhs = lhsof($rule);

  my $count = (exists $self->{counts}->{$rule}) ? $self->{counts}->{$rule} : 0;
  my $backoff_prob = prob_ind($self,$rule);
  my $total = $self->totals("rewrites");
  my $alpha = $self->alphas("rewrites");
  my $num = $count + $alpha * $backoff_prob;
  my $denom = $total + $alpha;

  # print "  PROB($rule) = ($count + $self->{alpha} * $base_prob) / $denom = ", ($num/$denom), $/;

  if (! exists $self->{size}->{$lhs}) {
    print "NO LHS $lhs\n";
    exit;
  }

  if ($verb) {
    print "PROB($rule)\n";
    print "  counts = $count\n";
    print "  alpha = $self->{alpha}\n";
    print "  size = $self->{size}->{$lhs}\n";
    print "  backoff prob = ", prob2($self,$rule), $/;
    print "  ", $self->{base_measure}->($self,$rule,1), $/;
    print "  NUM = $num\n";
    print "  DEN = $denom\n";
  }

  return $num / $denom;
}



sub alphas {
  my ($self,$which) = @_;

  die "* FATAL: no such alpha '$which'"
      if ($which ne "terminals" &&
          $which ne "rewrites" && 
          $which ne "nts" && 
          $which ne "pairs");
  return $self->{alphas}{$which};
}

sub totals {
  my ($self,$lhs,$which) = @_;

  die "* FATAL: no such total '$which'"
      if ($which ne "terminals" &&
          $which ne "rewrites" && 
          $which ne "nts" && 
          $which ne "pairs");
  return $self->{totals}{$which};
}

# draw from an lhs-specific Dirichlet distribution over terminals
sub prob_terminal {
  my ($self,$lhs,$rhs) = @_;

  my $alpha = $self->alphas("terminals");
  my $total = $self->totals($lhs,"terminals");
  my $denom = $alpha + $total;

  if (exists $self->{terminals}{$lhs}{$rhs}) {
    return 1.0 * ($self->{terminals}{$lhs}{$rhs}) / $denom;
  } else {
    return 1.0 * $alpha / $denom;
  }
}

# draw from an lhs-specific DP over nonterminal symbols
sub prob_pair {
  my ($self,$lhs,$rhs) = @_;

  # prob = ((count of pair) + alpha * (nt-specific stick prob)) / (total(lhs) + alph)
  my $count = (exists $self->{pairs}{$lhs}{$rhs}) ? $self->{pairs}{$lhs}{$rhs} : 0;
  my $alpha = $self->alphas("pair");
  my $total = $self->totals($lhs,"pairs");
  my $gem_prob = $self->prob_gem($rhs);
  my $num = $count + $alpha * $gem_prob;
  my $denom = $total + $alpha;

  # print "  PROB($rule) = ($count + $self->{alpha} * $base_prob) / $denom = ", ($num/$denom), $/;

  # if (! exists $self->{size}->{$lhs}) {
  #   print "NO LHS $lhs\n";
  #   exit;
  # }

  return $num / $denom;
}

# the probability that a nonterminal rewrites as another nonterminal
# (vs. rewriting as a terminal)
sub prob_ruletype {
  my ($self,$lhs) = @_;

  my $as_nonterm = $self->totals($lhs,"pairs") + 1;
  my $as_term = $self->totals($lhs,"terminals") + 1;
  return (1.0 * $as_nonterm / ($as_nonterm + $as_term));
  # my $alpha = $self->alphas("ruletype");
  # return (1.0 * ($as_nonterm + 0.5 * $alpha) / ($as_nonterm + $as_term + $alpha));
}

# for a rule N -> rhs, returns the result of \prod_{k \in rhs} p_N
# P_N(N -> k) + (1-p_N) P_V(N -> k)
sub prob_ind {
  my ($self,$rule,$verb) = @_;

  my ($lhs,@rhs) = split(" ",$rule);
  $lhs     =~ s/^\(//;
  $rhs[-1] =~ s/\)$//;

  my $weight = $self->prob_ruletype($lhs);

  my $prob = 1.0;
  foreach my $kid (@rhs) {
    if (is_terminal($kid)) {
      $prob *= (1.0 - $weight) * $self->prob_terminal($lhs,$kid);
    } else {
      $prob *= ($weight) * $self->prob_pair($lhs,$kid) 
    }
  }
  return $prob;
}

# assumes the nonterminals exist; creating new ones for renaming has
# to occur elsewhere, at the top level
sub prob_gem {
  my ($self,$k) = @_;

  return $self->{nts}{$k};
}


# geometric distribution over the number of children
# sub base_prob {
#   my ($self,$rule,$verb) = @_;

#   my @tokens = split(' ',$rule);
#   my $numkids = scalar(@tokens) - 1;

#   return $self->{stop} * (1.0-$self->{stop}) ** ($numkids-1);
# }

sub compress_files {
  map { system("bzip2 -9 $_") } @_;
}

sub dump_corpus {
  my ($self,$dir) = @_;
  mkdir $dir unless -d $dir;

  my @corpus = map { build_subtree_oneline($_) } @{$self->{corpus}};

  my $file = "$dir/corpus";
  open DUMP, ">$file" or warn "can't dump to $file";
  map { print DUMP $_, $/ } @corpus;
  close DUMP;
  compress_files($file);
}

sub dump_counts {
  my ($self,$dir) = @_;
  mkdir $dir unless -d $dir;

  my $file = "$dir/counts";
  open DUMP, ">$file" or warn "can't dump to $file";
  while (my ($subtree,$count) = each %{$self->{counts}}) {
    next unless $count > 0;
    print DUMP "$count $subtree\n";
  }
  close DUMP;
  compress_files($file);
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

# takes an array of length N whose elements are probabilities (the
# PDF, not the CDF!); if they don't sum to 1, N can be chosen, which
# could be used to extend the array
sub random_multinomial {
  my ($self) = @_;

  my $alpha = $self->alphas("nts");
  my $total = $self->totals("nts") + $alpha;
  my @pdf    = map { $_ / $total } (@${$self->{nts}},$alpha);

  my $prob = rand;

  my $len = scalar @pdf;
  my $sum = 0.0;
  my $which = 0;
  for (;;) {
    $sum += $pdf[$which];
    last if $sum > $prob;
    $which++;
    last if $which >= $len;
  }

  return $which;
}


1;

