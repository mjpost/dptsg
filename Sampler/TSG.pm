# Matt Post <post@cs.rochester.edu>

# This package implements transitioning from conditional distributions
# built over a DP prior, used in the Gibbs sampler.

package Sampler::TSG;

use strict;
use Sampler;
use Exporter;
use vars qw|$VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS|;

# our($lexicon,%rules,%deps,%PARAMS,$base_measure);

@ISA = qw|Exporter Sampler|;
@EXPORT = ();
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

my (%rewrites,%totals);

# constructor
#
# The main work of the constructor is in handling the base measure.
# If the caller requests the unordered base measure, then we have to
# do some post-processing on the base distribution grammar to make
# equivalence classes out of PCFG rules whose righthand sides are the
# same when viewed as a multiset.
sub new {
  my ($class,@params) = @_;

  # defaults
  my %defaults = (
    alpha => 100,
  );

  # assign defaults
  my %params = @params;
  map { $params{$_} = $defaults{$_} unless exists $params{$_} } (keys %params);

  # call parent
  my $self = $class->SUPER::new(%params);

  # if "unordered" is requested, that means we treat the PCFG rules
  # for the base measure as rewriting as multisets, so we need to
  # collapse them here and renormalize them
  if ($params{unordered}) {
    # set probs for canonical forms of rules for base measure
    my $before = scalar keys %{$self->{rules}};
    $self->{rules} = {};
    die "no rules passed in!" unless exists $params{rules};
    my %lhs;
    while (my ($rule,$weight) = each %{$params{rules}}) {
      my ($lhs) = split(' ',$rule);
      $lhs{$lhs} += $weight;

      my $canon = tocanonical($rule);
      $self->{rules}->{$canon} += $weight;
    }
    my $after = scalar keys %{$self->{rules}};

    print "* canonicalization of base measure rules made $after rules from $before\n";

    # normalize
    while (my ($rule,$weight) = each %{$self->{rules}}) {
      my ($lhs) = split(" ",$rule);
      $self->{rules}->{$rule} = $weight / $lhs{$lhs};
    }
  }

  # create other variables
  # $self->{totals} = {};
  # $self->{rewrites} = {};   # counts of lhs -> {N U T}* (a string of terms and nts)

  bless($self,$class);

  return $self;
}

# return the number of TSG rules by type
sub types {
  my ($self) = @_;

  return scalar keys %rewrites;
}

# return the number of TSG rules by token
sub tokens {
  my ($self) = @_;

  return sum values %rewrites;
}

# counts all of the rules in a tree by updating $hash
# will also update a counter ($size reference) if available
sub count {
  my ($self) = @_;
  my $corpus = $self->{corpus};

  # $self->{totals} = {};
  # $self->{rewrites} = {};
  %totals = ();
  %rewrites = ();

  $self->{splits} = 0;
  $self->{merges} = 0;

  my $count = sub {
    my ($node) = @_;
    my $lhs = $node->{label};
    if (@{$node->{children}} && $lhs !~ /^\*/) {
      my $rule = ruleof($node);
      $rewrites{$rule}++;
      $totals{$lhs}++;
      # $self->{rewrites}{$rule}++;
      # $self->{totals}{$lhs}++;
    }
  };

  # count all the subtrees in the corpus
  map { walk($_,[$count]) } @$corpus;
}

my $debug = 0;
my $loghandle;

# whether a node is marked as internal to a subtree
sub is_internal {
  my ($node) = @_;
  return ($node->{label} =~ /^\*/) ? 1 : 0;
}

sub make_internal {
  my ($node,$which) = @_;
  $which = 1 unless defined $which;
  if ($which) {  # make internal
    # make internal unless it already is
    $node->{label} = "*$node->{label}"
        unless is_internal($node);
  } else {  # make external
    # make external if it is currently internal
    $node->{label} =~ s/^\*//
        if is_internal($node);
  }
}

# samples two nodes at the same time for faster mixing
sub sample_two {
  my ($node,$self,$top) = @_;

  # skip the root node and leaf nodes
  return $node if $node->{label} eq "TOP";
  return undef unless @{$node->{children}};

  # print "SAMPLE_TWO($node->{label},$top->{label})\n";

  my ($outside,$inside,$merged);

  # choose a random child from the non-terminal children
  my @available;
  map { push(@available,$_) unless is_terminal($_) } @{$node->{children}};
  return undef unless scalar(@available);
  my $kid = $available[ rand(@available) ];

  my @was_internal = (
    is_internal($node),
    is_internal($kid));

  # print "NODE: $node->{label} ($was_internal[0]) KID: $kid->{label} ($was_internal[1])\n";

  # the four hypotheses vary based on whether $node and $kid are marked; 
  # to compute these hyps, we need the probability of 6 different pieces
  # 1. reps[0][1][1] top through $node and $kid
  # 2. reps[0][1][0] top through $node stopping at $kid
  # 3. reps[0][0] top stopping at $node
  # 4. reps[1][1] $node through $kid
  # 5. reps[1][0] $node stopping at $kid
  # 6. reps[2] $kid
  my @reps;

  use constant {
    TOP => 0,
    NODE => 1,
    KID => 2,
    BOTTOM => 3
  };

  my $current_state;
  my $current_state_str = "$top->{label} $node->{label} $kid->{label}";
  if ($was_internal[0] and $was_internal[1]) {
    # one rule
    $current_state = 3;
  } elsif ($was_internal[0]) {
    # two rules, break at kid
    $current_state = 1;
  } elsif ($was_internal[1]) {
    # two rules, break at node
    $current_state = 2;
  } else {
    # three rules
    $current_state = 0;
  }

  # decrement the counts
  decrement(\%rewrites, rep($top)->{str});
  decrement(\%totals, $top->{label});
  # print "  -" . rep($top)->{str} . $/;
  if (! $was_internal[0]) {
    decrement(\%rewrites, rep($node)->{str});
    # print "  -" . rep($node)->{str} . $/;
    decrement(\%totals,$node->{label});
  }
  if (! $was_internal[1]) {
    decrement(\%rewrites, rep($kid)->{str});
    # print "  -" . rep($kid)->{str} . $/;
    decrement(\%totals,$kid->{label});
  }

  # now compute the rest of the six pieces
  make_internal($node,1);
  make_internal($kid, 1);
  $reps[TOP][BOTTOM] = rep($top);

  make_internal($node,1);
  make_internal($kid, 0);
  $reps[TOP][KID] = rep($top); 
  $reps[KID][BOTTOM] = rep($kid); 

  make_internal($node,0);
  make_internal($kid, 1);
  $reps[TOP][NODE] = rep($top);  
  $reps[NODE][BOTTOM] = rep($node);  

  make_internal($node,0);
  make_internal($kid, 0);
  $reps[NODE][KID] = rep($node);  

  # compute the probabilities
  # (0,0) all external
  my @probs = (
    # three separate rules
    $self->prob($reps[TOP][NODE])
    * $self->prob($reps[NODE][KID])
    * $self->prob($reps[KID][BOTTOM]),
    # two rules, break at kid
    $self->prob($reps[TOP][KID])
    * $self->prob($reps[KID][BOTTOM]),
    # two rules, break at node
    $self->prob($reps[TOP][NODE])
    * $self->prob($reps[NODE][BOTTOM]),
    # one rule
    $self->prob($reps[TOP][BOTTOM])
      );

  my $next_state = random_multinomial(\@probs);

  if ($next_state == 0) {
    # this state corresponds to three separate rules

    # update counts
    $rewrites{$reps[TOP][NODE]->{str}}++;
    $rewrites{$reps[NODE][KID]->{str}}++;
    $rewrites{$reps[KID][BOTTOM]->{str}}++;
    $totals{$top->{label}}++;
    $totals{$node->{label}}++;
    $totals{$kid->{label}}++;

    # update states
    make_internal($node,0);
    make_internal($kid,0);
  } elsif ($next_state == 1) {
    # two rules, break at kid
    
    # update counts
    $rewrites{$reps[TOP][KID]->{str}}++;
    $totals{$top->{label}}++;
    $rewrites{$reps[KID][BOTTOM]->{str}}++;
    $totals{$kid->{label}}++;

    # update states
    make_internal($node,1);
    make_internal($kid,0);

  } elsif ($next_state == 2) {
    # two rules, break at node

    # update counts
    $rewrites{$reps[TOP][NODE]->{str}}++;
    $totals{$top->{label}}++;
    $rewrites{$reps[NODE][BOTTOM]->{str}}++;
    $totals{$node->{label}}++;

    # update states
    make_internal($node,0);
    make_internal($kid, 1);

  } else { 
    # one rule

    # update counts
    $rewrites{$reps[TOP][BOTTOM]->{str}}++;
    $totals{$top->{label}}++;

    # update states
    make_internal($node,1);
    make_internal($kid, 1);
  }

  # if (my $fh = $self->{logfh}) {
  #   my $gorn = "";
  #   print $fh "$self->{treeno} $gorn (" . join(",",@probs) . ") $current_state -> $next_state\n";
  # }

  # if ($current_state != $next_state) {
  #   print "  0: $reps[TOP][NODE]->{str} $reps[NODE][KID]->{str} $reps[KID][BOTTOM]->{str}\n";
  #   print "  1: $reps[TOP][KID]->{str} $reps[KID][BOTTOM]->{str}\n";
  #   print "  2: $reps[TOP][NODE]->{str} $reps[NODE][BOTTOM]->{str}\n";
  #   print "  3: $reps[TOP][BOTTOM]->{str}\n";
  #   my $sum = sum(@probs);
  #   my @normalized_probs = map { $_ / $sum } @probs;
  #   print "  SUMMARY $self->{treeno} (" . join(",",@normalized_probs) . ") $current_state -> $next_state\n";
  #   print "  FROM $current_state ($current_state_str) TO $next_state ($top->{label} $node->{label} $kid->{label})\n";
  # } else {
  #   print "  NO CHANGE\n";
  # }

  # print "  REWRITE COUNTS:\n";
  # map { print  "    $_ $rewrites{$_}\n" } keys %rewrites;
  # print "  LHS COUNTS:\n";
  # map { print  "    $_ $totals{$_}\n"   } keys %totals;

  # The same topnode will be the topnode for the children if $node
  # remains an internal node; else, it will be the current node
  return (is_internal($node)) ? $top : $node;
}

sub check_counts {
  my %my_totals;
  map { $my_totals{lhsof($_)} += $rewrites{$_} } keys %rewrites;
  while (my ($key,$val) = each %my_totals) {
    if ($totals{$key} != $val) {
      print "* WARNING: failed sanity check for '$key' (true count $val, cached $totals{$key})\n";
    }
  }

  return 1;
}

sub sample_each_TSG {
  my ($node,$self,$topnode) = @_;

  # skip the root node and leaf nodes
  return $node if $node->{label} eq "TOP";
  return undef unless @{$node->{children}};

  # print "SAMPLE_EACH_TSG($node->{label},$topnode->{label})\n";

  # base case: can't split leaves
  my $numkids = scalar @{$node->{children}};

  my ($outside,$inside,$merged);

  my $toplhs  = $topnode->{label};
  my $nodelhs = $node->{label};
  $nodelhs =~ s/^\*//;

  # is merged
  my $was_merged = ($node->{label} =~ /^\*/) ? 1 : 0;
  if ($was_merged) {
    # build the representations
    $merged = rep($topnode);
    $node->{label} = $nodelhs;
    $outside = rep($topnode);
    $inside = rep($node);

    # decrement the counts
    decrement(\%rewrites,$merged->{str});
    decrement(\%totals,$toplhs);

  } else {
    # build the representations
    $outside = rep($topnode);
    $inside = rep($node);
    $node->{label} = "*$nodelhs";
    $merged = rep($topnode);

    # decrease the counts
    decrement(\%rewrites,$inside->{str});
    decrement(\%totals,$nodelhs);
    decrement(\%rewrites,$outside->{str});
    decrement(\%totals,$toplhs);
  }

  my $outside_str = $outside->{str};
  my $inside_str = $inside->{str};
  my $merged_str = $merged->{str};

  # compute the three items with a combination of merging and removing
  # annotation
  if ($debug) {
    print "--\n";
    print "OUTSIDE: $outside_str\n";
    print "INSIDE:  $inside_str\n";
    print "MERGED: $merged_str\n";
  }

  # compute relative probability of merged vs. inside + outside
  my $prob_inside = $self->prob($inside);
  my $prob_outside = $self->prob($outside);
  my $prob_merged = $self->prob($merged);

  # transition with that possibility
  my $do_merge = (rand($prob_merged + $prob_inside * $prob_outside) < $prob_merged) ? 1 : 0;

  # print "MERGE $outside_str $inside_str (was=$was_merged is=$do_merge)\n";
  # print "  - $merged_str ($prob_merged)\n";
  # print "  - $outside_str ($prob_outside)\n";
  # print "  - $inside_str ($prob_inside)\n";

  if (my $fh = $self->{logfh}) {
    my $gorn = "";
    print $fh "$self->{treeno} $gorn $prob_outside $prob_inside $prob_merged $was_merged $do_merge\n";
  }

  if ($do_merge) {
    $self->{merges}++ unless $was_merged;

    $rewrites{$merged_str}++;
    $totals{$toplhs}++;

    # we need to make sure the current node is annotated with an
    # asterisk (indicating it's an internal node), and the asterisk is
    # not there if it was there before
    $node->{label} = "*$nodelhs";
  } else {  # not merging
    $self->{splits}++ if $was_merged;

    $rewrites{$outside_str}++;
    $totals{$toplhs}++;
    $rewrites{$inside_str}++;
    $totals{$nodelhs}++;

    # we need to clear the asterisk, which is there now if it wasn't
    # there before
    $node->{label} = $nodelhs;
  }
  
  # If we merged (or stayed merged), the same topnode will continue to
  # be the topnode.  If we are not merged, then the current node is
  # the root of (potential) trees below it
  return ($do_merge) ? $topnode : $node;
}

sub prob {
  my ($self,$rep) = @_;

  my $lhs = $rep->{label};
  my $rule = $rep->{str};

  # my $count = (exists $self->{rewrites}{$rule}) 
  #     ? $self->{rewrites}{$rule} : 0;
  my $count = (exists $rewrites{$rule}) 
      ? $rewrites{$rule} : 0;

  # my $total = (exists $self->{totals}{$lhs})
  #     ? $self->{totals}{$lhs} : 0;
  my $total = (exists $totals{$lhs})
      ? $totals{$lhs} : 0;

  my $base = $self->base_prob($rep);
  my $num = $count + $self->{alpha} * $base;
  my $denom = $total + $self->{alpha};
  
  # print "UNDEF($lhs) TOTALS\n" unless defined $self->{totals}->{$lhs};
  # print "UNDEF($rule,$lhs) ALPHA\n" unless defined $self->{alpha};

  my $prob = $num / $denom;

  if ($self->{verbosity} >= 3) {
    print "PROB($rule) = $prob\n";
    print "  lhs = $lhs\n";
    print "  counts = $count\n";
    print "  alpha = $self->{alpha}\n";
    print "  base_prob = $base\n";
    print "  size = $total\n";
    print "  NUM = $num\n";
    print "  DEN = $denom\n";
  }

  return $prob;
}

sub efficient_base_probs {
  my ($self,$orules,$irules) = @_;

  my $numorules = scalar @$irules;
  my $numirules = scalar @$irules;
  my $nummrules = $numorules + $numirules;

  my $opr = ((1.0 - $self->{stop}) ** ($numorules - 1)) * $self->{stop};
  my $ipr = ((1.0 - $self->{stop}) ** ($numirules - 1)) * $self->{stop};
  my $mpr = ((1.0 - $self->{stop}) ** ($nummrules - 1)) * $self->{stop};

  foreach my $rule (@$orules) {
    # print "OUTSIDE($rule)\n";
    $opr *= $self->{rules}{$rule};
    $mpr *= $self->{rules}{$rule};
  }
  foreach my $rule (@$irules) {
    # print "INSIDE($rule)\n";
    $ipr *= $self->{rules}{$rule};
    $mpr *= $self->{rules}{$rule};
  }

  return ($opr,$ipr,$mpr);
}

# returns the base measure probability of the tree fragment
# memoize('base_prob');
sub base_prob {
  my ($self,$rep,$verb) = @_;

  my $numrules = $rep->{rulecount};

#   print "BASE_PROB: ", (scalar @{$rep->{rules}}), " rules:\n"
#       if $debug;

  my $pr = 1.0;
  foreach my $rule (@{$rep->{rules}}) {
    my $form = $self->{unordered} ? tocanonical($rule) : $rule;

    print " PROB($rule) = $self->{rules}->{$form}\n"
        if $debug;
    print "* WARNING: couldn't find rule '$form' in PCFG rules\n" unless exists $self->{rules}->{$form};
    $pr *= $self->{rules}->{$form};
  }
#   print "PR is $pr after multiplying together $rep->{rulecount} rules\n";

  my $prg = ((1.0 - $self->{stop}) ** ($numrules - 1)) * $self->{stop};

  # if ($verb) {
    # my $ps = $self->{stop};
    # my $ps2 = 1.0 - $ps;
    # my $rules = $numrules - 1;
    # print "BASE_PROB() = $pr * $prg ($ps2 ** $numrules * $ps) = ", $pr * $prg,$/;
  # }

  return $pr * $prg;
}


# Returns the base measure probability of the tree fragment.
# This version
sub base_prob_2 {
  my ($self,$rep,$verb) = @_;

  my $numrules = $rep->{rulecount};

#   print "BASE_PROB: ", (scalar @{$rep->{rules}}), " rules:\n"
#       if $debug;

  my $pr = 1.0;
  foreach my $rule (@{$rep->{rules}}) {
    print " PROB($rule) = $self->{rules}->{$rule}\n"
        if $debug;
    print "* WARNING: couldn't find rule '$rule' in PCFG rules\n" unless exists $self->{rules}->{$rule};
    $pr *= $self->{rules}->{$rule};
  }
#   print "PR is $pr after multiplying together $rep->{rulecount} rules\n";

  my $prg = ((1.0 - $self->{stop}) ** ($numrules - 1)) * $self->{stop};

  if ($verb) {
    my $ps = $self->{stop};
    my $ps2 = 1.0 - $ps;
    my $rules = $numrules - 1;
    # print "BASE_PROB() = $pr * $prg ($ps2 ** $numrules * $ps) = ", $pr * $prg,$/;
  }

  return $pr * $prg;
}

sub likelihood {
  my ($self) = @_;

  my $sum = 0.0;
  foreach my $tree (@{$self->{corpus}}) {
    my @rules;
    extract_subtrees($tree,\@rules);
    my $prod = 1.0;
    foreach my $rule (@rules) {
      if (exists $totals{rule}) {
        $prod *= 1.0 * $rewrites{$rule} / $totals{lhsof($rule)};
      } else {
        print STDERR "* WARNING: likelihood() found no rule '$rule'\n";
      }
    }
    
    $sum += log($prod);
  }

  return $sum / log(2.0);
}

sub dump_counts {
  my ($self,$dir) = @_;
  mkdir $dir unless -d $dir;

  my $file = "$dir/counts";
  open DUMP, ">$file" or warn "can't dump to $file";
  # while (my ($subtree,$count) = each %{$self->{rewrites}}) {
  while (my ($subtree,$count) = each %rewrites) {
    next unless $count > 0;
    print DUMP "$count $subtree\n";
  }
  close DUMP;
  compress_files($file);
}

# takes a rule represented in paren form and transforms the RHS
# as a multiset
sub tocanonical {
  my ($rule) = @_;

  $rule =~ s/^\(|\)$//g;
  die "multilevel rule! ($rule)" if ($rule =~ /[\(\)]/);
  
  my ($lhs,@rhs) = split(' ',$rule);
  my $rhs = join(" ",sort(@rhs));

  my $newrule = "($lhs $rhs)";

  # print "CANON($rule) -> $newrule\n";

  return $newrule;
}

# takes a node and recursively discovers the rules inside the subtree
# rooted at that node
sub rep {
  my ($node,$hash) = @_;
  $hash = { label => $node->{label}, lexcount => 0 } unless defined $hash;

  # update hash
  push @{$hash->{rules}}, ruleof($node,1);
  $hash->{rulecount}++;
  $hash->{numkids} += scalar @{$node->{children}} || 1;
  $hash->{numkids}-- unless 1 == $hash->{rulecount};

  $hash->{str} .= " " if $hash->{str};
  $hash->{str} .= "(" . $node->{label};

  if (scalar @{$node->{children}}) {
    foreach my $kid (@{$node->{children}}) {
      if ($kid->{label} =~ /^\*/) {
        # merged node, keep going
        rep($kid,$hash);
      } else {
        # unmerged node, end here
        $hash->{str} .= " " . $kid->{label};
        $hash->{frontier} .= " " if $hash->{frontier};
        $hash->{frontier} .= $kid->{label};
      }
    }
  } else {
    my $sig = $node->{label};

    $hash->{str} .= " $sig";
    $hash->{frontier} .= " " if $hash->{frontier};
    $hash->{frontier} .= $sig;

    $hash->{lexcount}++;
  }

  $hash->{str} .= ")";
  # remove merged node markers
  $hash->{str} =~ s/\(\*/(/g;

  return $hash;
}


1;

