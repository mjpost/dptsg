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

use tree;

sub new {
  my $class = shift;
  my %params = @_;
  my $self = { 
    alpha => 10,
    iters => 100,
    stop => 0.9,
    headed_rule => 0.9,
    rundir => $ENV{PWD},
    verbosity => 1,
    pairs => {},
    stops => {},
  };

  map { $self->{$_} = $params{$_} } keys %params;
  bless($self,$class);

  if ($self->{deps}) {
    $self->{base_measure} = \&base_prob_dep;
    load_deps($self);
  } else {
    $self->{base_measure} = \&base_prob;
  }

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
  $self->{counts} = {};
  $self->{size} = {};

  # functions to count events over the training corpus for initialization
  my $count = sub {
    my ($node) = @_;
    my $rule = "(" . $node->{label} . " " . $node->{frontier} . ")";
    $self->{rulecounts}{$rule}++;
    $self->{widths}{$node->{label}}{$node->{numkids}}++;
    if ($node->{label} !~ /^\*/) {
      $self->{counts}{rep($node)->{str}}++;
      $self->{size}{$node->{label}}++;
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
  foreach my $tree (@{$self->{corpus}}) {
    print "ITER $iter TREE $self->{treeno}\n" if $self->{verbosity};
    map { $self->sample_each_structure($_,rep($tree,$_)) } @{$tree->{children}};
    # map { $self->sample_each_tsg($_,rep($tree,$_)) } @{$tree->{children}};
    $self->{treeno}++;
  }
}

# decides whether to merge/split TSG subtrees
sub sample_each_tsg {
  my ($self,$tree,$outside,$gorn) = @_;
  $gorn = "0" unless $gorn;  # address

  # base case: can't split leaves
  my $numkids = scalar @{$tree->{children}};
  return unless $numkids;

  my $inside = rep($tree);

#   print "Rule '$inside->{str}' has deps ($inside->{head} -> $inside->{deps})\n"
#       if ($inside->{head});

  # old state
  my $was_merged = ($tree->{label} =~ /^\*/) ? 1 : 0;

  # compute the three items with a combination of merging and removing
  # annotation
  my $outside_str = $outside->{str};
  my $merged = merged_rep($outside->{top},$tree,$was_merged);
  my $merged_str = $merged->{str};
  my $inside_str = $inside->{str};
  if ($debug) {
    print "--\n";
    print "OUTSIDE: $outside->{str}\n";
    print "INSIDE:  $inside->{str}\n";
    print "MERGED: $merged->{str}\n";
  }

  # set counts of lhs to 0 (if not set) to prevent later "undefined" notices
  map { $self->{size}->{lhsof($_)} = 0 unless exists $self->{size}->{lhsof($_)} } ($outside_str, $inside_str, $merged_str);

  # decrement the counts of the state(s) under consideration
#   print "NODE $tree->{label}\n";
  if ($was_merged) {
#     print "  MERGED($merged_str) $counts{$merged_str} - 1\n";
    $self->{counts}->{$merged_str} -= 1;
    print "WARNING!! counts{$merged_str} < 0\n" if $self->{counts}->{$merged_str} < 0;
    delete $self->{counts}->{$merged_str} unless $self->{counts}->{$merged_str};
    $self->{size}->{lhsof($merged_str)} -= 1;
  } else {
#     print "  OUTSIDE($outside_str) $counts{$outside_str} - 1\n";
#     print "  INSIDE($inside_str) $counts{$inside_str} - 1\n";
    $self->{counts}->{$outside_str} -= 1;
    print "WARNING!! counts{$outside_str} < 0\n" if $self->{counts}->{$outside_str} < 0;
    delete $self->{counts}->{$outside_str} unless $self->{counts}->{$outside_str};
    $self->{counts}->{$inside_str} -= 1;
    print "WARNING!! counts{$inside_str} < 0\n" if $self->{counts}->{$inside_str} < 0;
    delete $self->{counts}->{$inside_str} unless $self->{counts}->{$inside_str};
    $self->{size}->{lhsof($outside_str)} -= 1;
    $self->{size}->{lhsof($inside_str)} -= 1;
  }

  my $co = exists $self->{counts}->{$outside_str} ? $self->{counts}->{$outside_str} : 0;
  my $ci = exists $self->{counts}->{$inside_str} ? $self->{counts}->{$inside_str} : 0;
  my $cm = exists $self->{counts}->{$merged_str} ? $self->{counts}->{$merged_str} : 0;

  # compute relative probability of merged vs. inside + outside
  my $prob_merged = $self->prob($merged);
  my $prob_inside = $self->prob($inside);
  my $prob_outside = $self->prob($outside);
  my $denom = $prob_merged + $prob_inside * $prob_outside;

  if ($denom <= 0) {
    print "\n--\nCONSIDERING NODE: $tree->{label} -> ", (join " ", (map { $_->{label} } @{$tree->{children}})), $/;

    print "-- FAIL REPORT\n";
    print "  was_merged = $was_merged\n";
    print "  OUTSIDE: $outside->{str} ($outside->{rulecount} rules) (", $self->prob($outside,1), ")\n";
    print "  INSIDE:  $inside->{str} ($inside->{rulecount} rules) (", $self->prob($inside,1), ")\n";
    print "  MERGED: $merged->{str} ($merged->{rulecount} rules) (", $self->prob($merged,1), ")\n";
  }

  my $merge_prob = $denom ? ($prob_merged / $denom) : 0;

  # transition with that possibility
  my $merging = rand_transition($merge_prob);

#   print "MERGE $merge_prob (merging=$merging)\n";
#   print "  - $merged_str ($prob_merged)\n";
#   print "  - $outside_str ($prob_outside)\n";
#   print "  - $inside_str ($prob_inside)\n";

#   print "merged to get: '$merged->{str}' [$merge_prob] ";
  print "CHOSE $merged->{str} ($merge_prob)\n" if $merging and $debug;
#   print $/;

  if ($self->{log}) {
    my $fh = $self->{log};
    print $fh "$self->{treeno} $gorn $merge_prob $co $ci $cm $was_merged $merging\n";
  }

  if ($merging) {
#     print "  ADDING MERGED $merged_str\n";
    $self->{counts}{$merged_str}++;
    $self->{size}->{lhsof($merged_str)} += 1;
    $tree->{label} = "*" . $tree->{label} unless $was_merged;
  } else {
#     print "  ADDING OUTSIDE $outside_str\n";
#     print "  ADDING INSIDE $inside_str\n";
    $self->{counts}{$outside_str}++;
    $self->{counts}{$inside_str}++;
    $self->{size}->{lhsof($outside_str)} += 1;
    $self->{size}->{lhsof($inside_str)} += 1;
    $tree->{label} =~ s/^\*// if $was_merged;
  }
  
  # recurse   
  my $kidno = 1;
  foreach my $kid (@{$tree->{children}}) {
    # compute the outside structure for this child.  In both cases,
    # this involves annotating the child node in the inside structure
    # to facilitate future (potential) merges.  If we decided to merge
    # on this node, we also merge that annotated inside structure into
    # the existing outside structure

    my $new_outside = $merging ? rep($outside->{top},$kid) : rep($tree,$kid);
    $self->sample_each_tsg($kid, $new_outside, "$gorn.$kidno");
    $kidno++;
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

sub is_preterminal {
  my ($node) = @_;
  # print "PRETERMINAL($node->{label})?\n";

  my $is = scalar @{$node->{children}} == 1 
      and scalar @{@{$node->{children}}[0]->{children}} == 0;

  return $is;
}


sub ruleof {
  my ($node) = @_;
  if (scalar @{$node->{children}}) {
    return "($node->{label} " . join(" ", map { $_->{label} } @{$node->{children}}) . ")";
  }
  return "($node->{label})";
}

# visits each node of a tree and decides whether to (a) delete the
# node, and moving its children up to its parent or (b) insert a node
# above some sequence of its children
sub sample_each_structure {
  my ($self,$tree,$outside,$gorn) = @_;
  $gorn = "0" unless $gorn;

  # print "SAMPLE_STRUCTURE($tree->{label})\n";

  # don't consider preterminal nodes
  return if is_preterminal($tree);

  my $numkids = $tree->{numkids};
  return unless $numkids;

  # 1. Randomly choose whether to delete each of the children,
  # quitting when a deletion decision is made.  To facilitate mixing,
  # we consider the children in a random order.
  my @kid_indexes = shuffle(0..$tree->{numkids}-1);
  foreach my $kidno (@kid_indexes) {
    my $kid = $tree->{children}[$kidno];
    my $numgrandkids = @{$kid->{children}};

    # don't delete preterminals
    next if ((1 == scalar @{$kid->{children}})
             and (0 == scalar @{$kid->{children}[0]->{children}}));

    # print "SAMPLING WITH ($tree->{label},$kid->{label})\n";

    # There are four cases to consider, based on whether the node and
    # the child being considered are each internal or external.  Based
    # on these four possibilities, there are two possible tree
    # structure outcomes.  The following code handles all four
    # scenarios.

    my $prob_delete = 1.0;
    my $prob_stay = 1.0;
    my (@old_reps,@new_reps);
    my $deleted_node;
    if (is_internal($tree) and is_internal($kid)) {
      my $current_rep = rep($outside->{top});
      decrement($self->{rulecounts},$current_rep->{str});

      push(@old_reps,$current_rep);
      $prob_stay = $self->prob($current_rep);

      $deleted_node = splice @{$tree->{children}}, $kidno, 1, @{$kid->{children}};
      my $new_rep = rep($outside->{top});
      push(@new_reps, $new_rep);
      $prob_delete = $self->prob($new_rep);

    } elsif (is_internal($tree)) {  # node internal but child not
      my $current_rep_node = rep($outside->{top});
      my $current_rep_kid  = rep($kid);
      decrement($self->{rulecounts},$current_rep_node->{str});
      decrement($self->{rulecounts},$current_rep_kid->{str});

      push(@old_reps,$current_rep_node,$current_rep_kid);
      $prob_stay = $self->prob($current_rep_node) * $self->prob($current_rep_kid);

      $deleted_node = splice @{$tree->{children}}, $kidno, 1, @{$kid->{children}};
      my $new_rep = rep($outside->{top});
      push(@new_reps, $new_rep);
      $prob_delete = $self->prob($new_rep);

    } elsif (is_internal($kid)) {   # child internal but node not
      my $current_rep = rep($tree);
      decrement($self->{rulecounts},$current_rep->{str});
      
      push(@old_reps,$current_rep);
      $prob_stay = $self->prob($current_rep);

      $deleted_node = splice @{$tree->{children}}, $kidno, 1, @{$kid->{children}};
      my $new_rep = rep($tree);
      push(@new_reps, $new_rep);
      $prob_delete = $self->prob($new_rep);

    } else {   # neither internal
      my $current_rep_node = rep($tree);
      my $current_rep_kid  = rep($kid);
      decrement($self->{rulecounts},$current_rep_node->{str});
      decrement($self->{rulecounts},$current_rep_kid->{str});
  
      push(@old_reps,$current_rep_node,$current_rep_kid);
      $prob_stay = $self->prob($current_rep_node) * $self->prob($current_rep_kid);

      $deleted_node = splice @{$tree->{children}}, $kidno, 1, @{$kid->{children}};
      my $new_rep = rep($tree);
      push(@new_reps, $new_rep);
      $prob_delete = $self->prob($new_rep);
    }
    
    my $transition_prob = ($prob_delete / ($prob_stay + $prob_delete));
    my $do_delete = rand_transition($transition_prob);

    if ($do_delete) {
      # print "  DELETING NODE $tree->{label}/[$kid->{label}]\n";

      # instead of having to update these, we should really just do
      # them through accessor functions
      $tree->{numkids} = scalar @{$tree->{children}};

      # increase the rule count
      map {
        $self->{rulecounts}{$_->{str}}++
      } @new_reps;

      # only delete one child at a time
      # last;
    } else {
      # put the node back
      my @grandkids = splice @{$tree->{children}}, $kidno, $numgrandkids, $deleted_node;

      # restore the counts
      map {
        $self->{rulecounts}{$_->{str}}++
      } @old_reps;
    }
  }

  # 2. Randomly decide whether to place a new node over each
  # contiguous span of children.  As before, quit after succeeding,
  # and randomize the spans we choose to facilitate mixing.
  if ($tree->{numkids} > 2) {
    # print "INSERT BENEATH($tree->{label})\n";

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

      # if this node is internal, then the newly created one should be, too
      my ($prob_stay,$prob_insert);
      my (@old_reps,@new_reps);
      my $newnode = { label => $tree->{label} };
      if (is_internal($tree)) {
        my $current_rep = rep($outside->{top});
        decrement($self->{rulecounts},$current_rep->{str});

        push(@old_reps,$current_rep);
        $prob_stay = $self->prob($current_rep);

        # create the new node and insert it over the children
        $newnode->{label} = "*" . $newnode->{label};
        my @deleted = splice(@{$tree->{children}},$s,$t-$s+1,$newnode);
        $newnode->{children} = \@deleted;

        my $new_rep = rep($outside->{top});
        push(@new_reps, $new_rep);
        $prob_insert = $self->prob($new_rep);

      } else {
        my $current_rep = rep($tree);
        decrement($self->{rulecounts},$current_rep->{str});

        push(@old_reps,$current_rep);
        $prob_stay = $self->prob($current_rep);

        # print "  BEFORE: ", ruleof($tree), " [$s,$t]$/";

        # create the new node and insert it over the children; the new
        # node is an internal node only if at least one of kids is an
        # internal node
        my @deleted = splice(@{$tree->{children}},$s,$t-$s+1,$newnode);
        $newnode->{children} = \@deleted;

        # print "  AFTER1: ", ruleof($tree), $/;
        # print "  AFTER2: ", ruleof($newnode), $/;

        my $any_internal_kids = scalar grep { is_internal($_) } @{$newnode->{children}};
        if ($any_internal_kids) {
          $newnode->{label} = "*" . $newnode->{label};
          my $new_rep = rep($tree);
          push(@new_reps, $new_rep);
          $prob_insert = $self->prob($tree);
        } else {
          my $new_rep_node = rep($tree);
          my $new_rep_kid  = rep($newnode);
          push(@new_reps,$new_rep_node,$new_rep_kid);
          $prob_insert = $self->prob($new_rep_node) * $self->prob($new_rep_kid);
        }
      }

      my $transition_prob = ($prob_insert / ($prob_insert + $prob_stay));
      my $do_insert = rand_transition($transition_prob);

      if ($do_insert) {
        # print "DOING INSERT\n";

        $tree->{numkids} = scalar @{$tree->{children}};
        $newnode->{numkids} = scalar @{$newnode->{children}};

        map {
          $self->{rulecounts}{$_->{str}}++
        } @new_reps;

        last;
      } else {
        # print "NOT DOING INSERT\n";

        # put the children back
        splice @{$tree->{children}}, $s, 1, @{$newnode->{children}};

        # restore the counts
        map {
          $self->{rulecounts}{$_->{str}}++
        } @new_reps;
      }
    }
  }

  # recurse   
  my $kidno = 1;
  foreach my $kid (@{$tree->{children}}) {
    # compute the outside structure for this child.  In both cases,
    # this involves annotating the child node in the inside structure
    # to facilitate future (potential) merges.  If we decided to merge
    # on this node, we also merge that annotated inside structure into
    # the existing outside structure

    my $new_outside = is_internal($tree) ? rep($outside->{top},$kid) : rep($tree,$kid);
    $self->sample_each_structure($kid, $new_outside, "$gorn.$kidno");
    $kidno++;
  }
}

sub rand_transition {
  my $prob = shift;
  return (rand() < $prob) ? 1 : 0;
}

# returns the probability of the tree fragment, sampled from the DP
sub prob {
  my ($self,$rep,$verb) = @_;

  my $lhs = lhsof($rep->{str});

  my $count = (exists $self->{counts}->{$rep->{str}}) ? $self->{counts}->{$rep->{str}} : 0;

  my $num = $count + $self->{alpha} * $self->{base_measure}->($self,$rep);
  my $denom = $self->{size}->{$lhs} + $self->{alpha};

  if ($verb) {
    print "PROB($rep->{str})\n";
    print "  counts = $count\n";
    print "  alpha = $self->{alpha}\n";
    print "  size = $self->{size}->{$lhs}\n";
    print "  base_prob = ", $self->{base_measure}->($self,$rep), $/;
    print "  ", $self->{base_measure}->($self,$rep,1), $/;
    print "  NUM = $num\n";
    print "  DEN = $denom\n";
  }

  return $num / $denom;
}

sub rule_prob {
  my ($self,$rule) = @_;

  # since we want to allow rules not seen in the training data, the
  # rule prob is markovized; an lhs generates N children with
  # probability Pr(# | lhs), and then chooses each of them
  # independently as Pr(rhs_i | lhs)

  $rule =~ s/^\(|\)$//g;
  my ($lhs,@rhs) = split(' ',$rule);
  die "no stops $lhs ($rule)" unless $self->{stops}{$lhs};
  my $pr = 1.0;
  map {
    $pr *= ($self->{pairs}{$lhs}{$_}) ? $self->{pairs}{$lhs}{$_} : 1e-12;
  } @rhs;
  my $numstops = scalar @rhs - 1;
  $pr *= (1.0-$self->{stops}{$lhs}) ** $numstops * $self->{stops}{$lhs};

  # old, static version
  # print "* WARNING: couldn't find rule '$rule' in PCFG rules\n" unless exists $self->{rules}->{$rule};
  # $pr *= $self->{rules}->{$rule};

  return $pr;
}

# returns the base measure probability of the tree fragment
# memoize('base_prob');
sub base_prob {
  my ($self,$rep,$verb) = @_;

  my $pr = 1.0;
#   print "BASE_PROB: ", (scalar @{$rep->{rules}}), " rules:\n"
#       if $debug;
  foreach my $rule (@{$rep->{rules}}) {
    print " PROB($rule) = $self->{rules}->{$rule}\n"
        if $debug;

    my $rule_prob = $self->rule_prob($rule);

  }
#   print "PR is $pr after multiplying together $rep->{rulecount} rules\n";

  my $prg = ((1.0 - $self->{stop}) ** ($rep->{rulecount} - 1)) * $self->{stop};

  if ($verb) {
    my $ps = $self->{stop};
    my $ps2 = 1.0 - $ps;
    my $rules = $rep->{rulecount} - 1;
    print "BASE_PROB($rep->{str}) = $pr * $prg ($ps2 ** $rules * $ps) = ", $pr * $prg ;
  }

  return $pr * $prg;
}

# memoize('base_prob_dep');
sub base_prob_dep {
  my ($self,$rep,$verb) = @_;
  $verb = 0 unless $verb;
  my $head = delex($rep->{head});

  my @rule_probs = map { $self->{rules}->{$_} || warn "no rule for $_" } @{$rep->{rules}};
  my $prr = reduce { $a * $b } @rule_probs;
  my $prg = ((1.0 - $self->{stop}) ** ($rep->{rulecount} - 1)) * $self->{stop};
  
  my $prd;
  if ($head) {
    if (! exists $self->{deps}->{$head}{__stop__}) {
      print "FOUND NO STOP FOR $head\n";
      exit;
    }

    # prob of lexicalized subtree, prior prob of head
    my $prior = $self->{deps}->{$head}{__prior__} || warn "no PRIOR for '$head'\n";
    $prd = $self->{headed_rule} * $prior;
    # prob of head generating that number of deps
    if ($rep->{deps}) {
      my @deps = map { delex($_) } (split ' ', $rep->{deps});
      my @dep_probs = map { $self->prob_dep($head,$_) } @deps;
      # prob of generating individual dependencies
      $prd *= reduce { $a * $b } @dep_probs;
      # prob of generating that number of them
      $prd *= (1.0-$self->{deps}->{$head}{__stop__})**(scalar @deps);
    }
    # and finally the prob of stopping
    $prd *= ($self->{deps}->{$head}{__stop__});
  } else {
    # prob of unlexicalized subtree
    $prd = (1.0 - $self->{headed_rule});
  }

  if ($verb > 1) {
    my $ps = $self->{stop};
    my $ps2 = 1.0 - $ps;
    my $rules = $rep->{rulecount} - 1;
    print " BASE_PROB_DEP($rep->{str}) = ", $prr * $prg * $prd, $/;
    print "  RULE PROBS: ", join(" ", @rule_probs), $/;
    print "  PR = $prr\n";
    print "  PRG = $prg ($ps2 ** $rules * $ps)\n";
    print "  PRD = $prd\n";
    if ($head) {
      print "    head prior = ", $self->{deps}->{head}{__prior__}, $/;
      print "    headed rule = $self->{headed_rule}\n";
      print "    stop prob = ", $self->{deps}->{$head}{__stop__}, $/;
      if ($rep->{deps}) {
        my @deps = map { delex($_) } (split ' ',$rep->{deps});
        my $numdeps = @deps;
        print "    $numdeps DEPS\n";
        my @dep_probs = map { $self->prob_dep($head,$_) } @deps;
        foreach my $dep (@deps) {
          print "    DEP: $head -> $dep (", $self->prob_dep($head,$dep), ")\n";
        }
        my $count = (exists $self->{counts}{$rep->{str}}) ? $self->{counts}{$rep->{str}} : 0;
        print "    COUNTS($rep->{str}) = $count\n";
      }
    }
  }
  return $prr * $prg * $prd;
}

sub prob_dep {
  my ($self,$head,$dep) = @_;

#   print "PROB_DEP($head,$dep)\n";

  if (exists $self->{deps}->{$head}{$dep}) {
#     print "  FOUND: $self->{deps}->{$head}{$dep}\n";
    return $self->{deps}->{$head}{$dep};
  } else {
#     print "  BACKOFF: $self->{deps}->{$head}{__backoff__} $self->{deps}->{$dep}{__prior__}\n";
    return $self->{deps}->{$head}{__backoff__} * $self->{deps}->{$dep}{__prior__};
  }
}


# Takes the top of a subtree, an internal node, and a marker indicating
# whether the internal node is already a merged node.  Computes the representation
# of the whole subtree starting at $tree assuming the node IS merged.  Resets
# the state of the merged node afterward
sub merged_rep {
  my ($top,$node,$was_merged) = @_;

  $node->{label} =~ s/^/\*/o unless $was_merged;
  my $rep = rep($top);
  $node->{label} =~ s/^\*//o unless $was_merged;

  return $rep;
}

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

1;
