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
    beta => {},
    iters => 100,
    stop => 0.9,
    rundir => $ENV{PWD},
    verbosity => 1,
    alphas => {
      nts => 100,          # number of nonterminals
      terminals => 100,
      pairs => 100,
      rewrites => 100,
    },
    totals => {
      nts => 0,               # number of nodes in corpus
      terminals => {},        # total number of terminal rewrites for each lhs
      pairs => {},            # total number of nonterminal rewrites for each lhs
      rewrites => {},         # total number of rule rewrites for each lhs
      words => {}             # total number of words seen in the training data
    },
    nts => [],                # stick-breaking construction over nonterminals
    rewrites => {},           # counts of lhs -> {N U T}* (a string of terms and nts)
    pairs => {},              # counts of lhs -> N (a nonterminal)
    terminals => {},          # counts of lhs -> T (a terminal)
    words => {}
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
  $self->{totals}{words} = 0;
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
        if (islex($kid->{label})) {
          $self->{terminals}{$lhs}{$lab}++;
          $self->{totals}{terminals}{$lhs}++;
        } else {
          $self->{pairs}{$lhs}{$lab}++;
          $self->{totals}{pairs}{$lhs}++;
        }
      }
    } else {
      # terminal
      $self->{words}{$lhs}++;
      $self->{totals}{words}++;
    }
  };

  # walk over the nodes of each tree in the corpus, applying the
  # closures defined above to each of them
  map { walk($_,[$count]) } @$corpus;

  # we need to seed the set of nonterminals with alpha (added when the array is sampled, put a 0 there now)
  push(@{$self->{nts}},0);

  # print out the counts
  if ($self->{verbosity} > 2) {
    print "TOTAL nts $self->{totals}{nts}\n";
    foreach my $word (qw/rewrites pairs terminals/) {
      while (my ($lhs,$total) = each %{$self->{totals}{$word}}) {
        print "TOTAL $word $lhs $total\n"
      }
    }
    foreach my $lhs (0..@{$self->{nts}}-1) {
      print "COUNT nt $lhs $self->{nts}->[$lhs]\n";
    }
    foreach my $word (qw/rewrites pairs terminals/) {
      while (my ($lhs,$hash) = each %{$self->{$word}}) {
        while (my ($rhs,$val) = each %$hash) {
          print "COUNT $word $lhs $rhs $val\n"
        }
      }
    }
  }
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

  my @funcs = (\&sample_each_insert, \&sample_each_delete, \&sample_each_rename, sub { });
  # my @funcs = (\&sample_each_rename);

  $| = 1;
  foreach my $tree (@{$self->{corpus}}) {
    print "ITER $iter TREE $self->{treeno} ins:$self->{insertions} del:$self->{deletions} ren:$self->{renamings}\n" if $self->{verbosity} and (! ($self->{treeno} % 100));

    # $sub is an anonymous function that randomly chooses a function
    # to apply from the @funcs array; with this we can use the walk
    # function to randomly choose one of the functions to apply to
    # each node of each tree
    my $apply_random_func = sub {
      my $func = $funcs[rand(@funcs)];
      $func->(@_);
    };
    walk($tree, [$apply_random_func], $self);

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

# randomly inserts a new nonterminal over some sequence of children
sub sample_each_insert {
  my ($tree,$self) = @_;

  # print "SAMPLE_STRUCTURE($tree->{label})\n";

  # don't consider preterminal nodes
  return if is_preterminal($tree);

  # INSERTIONS Randomly choose a span and then sample from the
  # conditional distribution of nodes to insert (including inserting
  # no node).

  my $numkids = scalar @{$tree->{children}};
  if ($numkids) {
    # print "INSERT BENEATH($tree->{label})\n";

    my @span_indexes;
    for my $span (1..$tree->{numkids}-1) {
      for my $s (0..$tree->{numkids}-$span) {
        my $t = $s + $span - 1;
        push @span_indexes, [$s,$t];
      }
    }
    @span_indexes = shuffle(@span_indexes);

    for my $index (0..$#span_indexes) {
      my ($s,$t) = @{$span_indexes[$index]};

      # decrement counts
      my $oldrule = ruleof($tree);
      $self->subtract($oldrule);

      # print "INSERT($oldrule)[$s,$t]\n";

      # create the new node
      my $newnode = { };
      # replace the children corresponding to the selected span with
      # newnode, and put those children beneath newnode
      my @deleted = splice(@{$tree->{children}},$s,$t-$s+1,$newnode);
      $newnode->{children} = \@deleted;

      # build the conditional distribution
      my $num_nts = scalar @{$self->{nts}};
      my $prob_stay = $self->prob($oldrule);
      # add the probability of not inserting to distribution
      my @dist = ( $prob_stay );
      foreach my $nonterm (0..$num_nts-1) {
        $newnode->{label} = $nonterm;

        my $rule1 = ruleof($tree);
        my $rule2 = ruleof($newnode);
        my $prob = $self->prob($rule1) * $self->prob($rule2);

        push(@dist,$prob);
      }

      # map { print "* ", $_-1, " $dist[$_]\n" } (0..$#dist);

      # print "$self->{treeno} $do_insert PROB ", sprintf("%.3g",$prob_insert)," (INSERT ", sprintf("%.3g",$prob_insert)," STAY ",sprintf("%.3g",$prob_stay),")\n"
          # if $debug;

      # if do_insert is 0, that means we are not inserting; otherwise,
      # it means we have selected nonterminal ($do_insert-1)
      my $newlabel = random_multinomial(\@dist);
      if ($newlabel) {
        $self->{insertions}++;
        # print "DOING INSERT\n";

        $tree->{numkids} = scalar @{$tree->{children}};
        $newnode->{numkids} = scalar @{$newnode->{children}};

        map { $self->add($_) } (ruleof($tree),ruleof($newnode));

        # a new nonterminal was sampled
        if ($newlabel == $num_nts - 1) {
          $self->{nts}->[$newlabel-1] = 1;
          push(@{$self->{nts}}, 0);
        } else {
          $self->{nts}->[$newlabel-1]++;
        }

        $self->{totals}{nts}++;

      } else {
        # print "NOT DOING INSERT\n";

        # put the children back
        splice @{$tree->{children}}, $s, 1, @{$newnode->{children}};

        # restore the counts
        map { $self->add($_) } $oldrule;
      }

      # only try one span (keeping loop to make it easy to change this
      # if we decide to later on)
      last;
    }
  }
}

# randomly deletes a child
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
    $self->{totals}{nts}--;
    $self->subtract($kid_rule);
  
    my $deleted_node = splice @{$tree->{children}}, $kidno, 1, @{$kid->{children}};
    my $new_rule = ruleof($tree);

    my $prob_delete = $self->prob($new_rule);
    my $prob_stay = $self->prob($current_rule) * $self->prob($kid_rule);

    my $do_delete = (rand($prob_delete + $prob_stay) < $prob_delete) ? 1 : 0;

    if ($do_delete) {
      $self->{deletions}++;

      # print "  DELETING NODE $tree->{label}/[$kid->{label}]\n";

      # instead of having to update these, we should really just do
      # them through accessor functions
      $tree->{numkids} = scalar @{$tree->{children}};

      # increase the rule count
      map { $self->add($_) } ($new_rule);
    } else {
      # put the node back
      my @grandkids = splice @{$tree->{children}}, $kidno, $numgrandkids, $deleted_node;

      # restore the counts
      map { $self->add($_) } ($current_rule, $kid_rule);
      $self->{nts}->[$kid->{label}]++;
      $self->{totals}{nts}++;
    }

    # only try one span (keeping loop to make it easy to change this
    # if we decide to later on)
    last;
  }
}

# samples a new label for a child (many children) from the conditional distribution of labels
sub sample_each_rename {
  my ($tree,$self) = @_;

  # print "SAMPLE_STRUCTURE($tree->{label})\n";

  # don't consider preterminal nodes
  return if is_preterminal($tree);

  my $numkids = $tree->{numkids};
  return unless $numkids;

  # randomly pick a child to try to rename
  my @kid_indexes = shuffle(0..@{$tree->{children}}-1);
  foreach my $kidno (@kid_indexes) {
    my $kid = $tree->{children}[$kidno];

    my $numgrandkids = @{$kid->{children}};
    last unless $numgrandkids;

    # record the child's current label
    my $oldlabel = $kid->{label};

    # subtract the counts
    my $current_rule1 = ruleof($tree);
    my $current_rule2 = ruleof($kid);
    $self->subtract(ruleof($tree));
    $self->{nts}->[$kid->{label}]--;
    $self->subtract(ruleof($kid));

    # build the conditional distribution
    my $num_nts = @{$self->{nts}};
    my @dist;
    foreach my $nonterm (0..$num_nts-1) {
      $kid->{label} = $nonterm;
      my $rule1 = ruleof($tree);
      my $rule2 = ruleof($kid);
      my $prob = $self->prob($rule1) * $self->prob($rule2);
      push(@dist,$prob);

      print "* ", ruleof($tree), " ", ruleof($kid), " $prob\n"
          if $debug;
    }
    # and randomly sample from it
    my $newlabel = random_multinomial(\@dist);

    print "$self->{treeno} RENAME old $oldlabel ($dist[$oldlabel]) new $newlabel ($dist[$newlabel])\n"
        if $debug;

    # assign the new label
    $kid->{label} = $newlabel;

    # restore the counts
    map { $self->add($_) } (ruleof($tree),ruleof($kid));

    # if this is a newly generated nonterminal, set its count to one, and create a
    # new new nonterminal for possible later selection
    if ($newlabel == $num_nts - 1) {
      $self->{nts}->[$newlabel] = 1;
      push(@{$self->{nts}}, 0);
    } else {
      # otherwise if it's an existing label, increment its count
      $self->{nts}->[$newlabel]++;
    }
    
    if ($newlabel != $oldlabel) {
      $self->{renamings}++;
    }

    # only try one child (kept in loop, though, in case we change this later)
    last;
  }
}

# do nothing!
sub noop {
}

# adds counts of the given $rule
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
    if (islex($rhs)) {
      # terminals
      $self->{terminals}{$lhs}{$rhs}++;
      $self->{totals}{terminals}{$lhs}++;    
    } else { 
      # pairs
      $self->{pairs}{$lhs}{$rhs}++;
      $self->{totals}{pairs}{$lhs}++;
    }
  }
}

# subtracts counts of the given $rule
sub subtract {
  my ($self,$rule) = @_;
  
  # print "SUBTRACT($rule)\n";
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

  my $count = (exists $self->{rewrites}{$lhs}{$rule}) ? $self->{rewrites}{$lhs}{$rule} : 0;
  my $base_prob = prob_ind($self,$rule);
  my $total = $self->totals($lhs,"rewrites");
  my $alpha = $self->alphas("rewrites");
  my $num = $count + $alpha * $base_prob;
  my $denom = $total + $alpha;

  print "PROB($rule) = ($count + $self->{alpha} * $base_prob) / $denom = ", ($num/$denom), $/, "--\n" 
      if $debug;

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
          $which ne "pairs");

  my $count = $self->{totals}{$which}{$lhs} || 0;
  # print "TOTALS($lhs,$which) = $count\n";
  return $count;
}

sub base_prob_terminal {
  my ($self,$word) = @_;

  my $count = $self->{words}{$word};
  my $total = $self->{totals}{words};

  my $prob = 1.0 * $count / $total;
  print "BASE_PROB_TERMINAL($word) = $count / $total = $prob\n"
      if $debug;

  return $prob;
}

# draw from an lhs-specific Dirichlet distribution over terminals
sub prob_terminal {
  my ($self,$lhs,$rhs) = @_;

  my $alpha = $self->alphas("terminals");
  my $base_prob = $self->base_prob_terminal($rhs);
  my $total = $self->totals($lhs,"terminals");
  my $denom = $alpha + $total;

  if (exists $self->{terminals}{$lhs}{$rhs}) {
    my $count = $self->{terminals}{$lhs}{$rhs};
    my $prob = ($count + $alpha * $base_prob) / $denom;
    print "PROB_TERMINAL($lhs -> $rhs) = ($count + $alpha * $base_prob) / $denom = $prob\n"
        if $debug;
    return $prob;
  } else {
    my $prob = 1.0 * $alpha * $base_prob / $denom;
    print "PROB_TERMINAL($lhs -> $rhs) = 0 + $alpha * $base_prob / $denom = $prob\n"
        if $debug;
    return $prob;
  }
}

# draw from an lhs-specific DP over nonterminal symbols
sub prob_pair {
  my ($self,$lhs,$rhs) = @_;

  # prob = ((count of pair) + alpha * (nt-specific stick prob)) / (total(lhs) + alph)
  my $count = (exists $self->{pairs}{$lhs}{$rhs}) ? $self->{pairs}{$lhs}{$rhs} : 0;
  my $alpha = $self->alphas("pairs");
  my $total = $self->totals($lhs,"pairs");
  my $gem_prob = $self->prob_gem($rhs);
  my $num = $count + $alpha * $gem_prob;
  my $denom = $total + $alpha;

  print "PROB_PAIR($lhs -> $rhs) = ($count + $alpha * $gem_prob) / $denom = ", ($num/$denom), $/
      if $debug;

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
  my $prob = (1.0 * $as_nonterm / ($as_nonterm + $as_term));

  print "PROB_RULETYPE($lhs) = $as_nonterm / ($as_nonterm + $as_term) = $prob\n"
      if $debug;

  return $prob;
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

  my $rank = scalar @rhs;
  my $prob = $self->{stop} * (1.0 - $self->{stop}) ** ($rank - 1);
  foreach my $rhs (@rhs) {
    if (islex($rhs)) {
      my $pt = $self->prob_terminal($lhs,$rhs);
      $prob *= (1.0 - $weight) * $pt;
    } else {
      my $pp = $self->prob_pair($lhs,$rhs);
      $prob *= ($weight) * $pp;
    }
  }

  print "PROB_IND($rule) = $prob\n"
      if $debug;

  return $prob;
}

# assumes the nonterminals exist; creating new ones for renaming has
# to occur elsewhere, at the top level
sub prob_gem {
  my ($self,$k) = @_;

  my $alpha = $self->alphas("nts");
  my $len = scalar @{$self->{nts}};
  my $count = $self->{nts}->[$k] + $alpha;
  my $total = $self->{totals}{nts} + $alpha * $len;

  my $prob = $count / $total;

  print "PROB_GEM($k) = $count / $total = $prob\n"
      if $debug;

  return $prob;
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

  my @corpus = map { build_subtree_oneline($_,1) } @{$self->{corpus}};

  my $file = "$dir/corpus";
  open DUMP, ">$file" or warn "can't dump to $file";
  map { print DUMP $_, $/ } @corpus;
  close DUMP;
  compress_files($file);
}

sub dump_counts {
  print "dump_counts() not implemented!\n";
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

1;

