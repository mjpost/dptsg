#!/usr/bin/perl

# extracts the baseline DOP grammar as described in
# "Do all fragments count?" (Bod, 2003).
# this script actually just extracts all depth 2+ fragments

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpdop";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use List::Util qw|max|;
use TSG;

my $min_height  = shift || 2;
my $max_height  = shift || 14;
my $num_samples = shift || 400000;
my $lex_file = shift || "$basedir/data/lex.02-21";

print STDERR "$num_samples from heights $min_height to $max_height\n";
print STDERR "lexicon = $lex_file\n";

my $lexicon = read_lexicon($lex_file);

my (@corpus);
while (my $line = <>) {
  chomp $line;

  my $tree = build_tree($line,0,$lexicon);
  mark_height($tree);
  push @corpus, $tree;
}

foreach my $height ($min_height..$max_height) {
  my $skipped = 0;
  my %rules;
  for my $i (1..$num_samples) {
    # 1. pick a random tree that's tall enough
    my $tree;
    do {
      $tree = $corpus[int(rand @corpus)];
    } while ($tree->{height} < $height);

    # 2. pick a random eligible starting node
    my @nodes;
    get_nodes($tree,\@nodes,$height);
    my $node = $nodes[int(rand @nodes)];
    # 2b. now reset nodes so it contains just eligible descendants of $node
    @nodes = @{$node->{children}};

    # 3. randomly expand it until we have something of height $height
#     print "Expanding ", rep($node)->{str}, $/;
    while (height($node) < $height) {

      my $nextnode = splice @nodes, int(rand @nodes), 1;
      $nextnode->{label} = "*" . $nextnode->{label};

      push @nodes, @{$nextnode->{children}};
    }

    my $rulestr = rep($node)->{str};

    # prune the subtree if it doesn't meet the requirements of Bod (2001)
    # (i.e., > 12 lexical items, unlexicalized with height > 6)
    my $subtree = build_subtree($rulestr);
    mark_subtree_height($subtree);

    die "height($rulestr) = $subtree->{height} != height ($height) (node height = ", height($node), ")!"
        unless $subtree->{height} == $height;
    my $numterms = count_subtree_lex($subtree);
    if ($numterms > 12 || ($numterms == 0 && $height > 6)) {
#       print "SKIPPING $numterms $rulestr\n";
      $skipped++;
    } else { 
      # sample the rule
      $rules{$rulestr}++;
    }

    # clean the tree for reuse
    clean($node);
  }

  while (my ($rule,$count) = each %rules) {
    print "$height $count $rule\n"
  }
  print STDERR "height $height skipped $skipped\n";
}


# foreach my $height (2..$max_height) {
#   while (my ($rule,$count) = each %{$rules[$height]}) {
#     print "$height $count $rule\n"
#   }
# }


## subroutines #######################################################

sub get_nodes {
  my ($subtree,$array,$height) = @_;

  if (! defined $height or $subtree->{height} >= $height) {
    push @$array, $subtree;
    map { get_nodes($_,$array,$height) } @{$subtree->{children}};
  }
}

sub mark_height {
  my $node = shift;

  if (! @{$node->{children}}) {
    $node->{height} = 1;
    return $node;
  }    

  map { mark_height($_) } @{$node->{children}};

  $node->{height} = 1 + max( map { $_->{height} } @{$node->{children}});
  return $node;
}

sub height {
  my ($node,$istop) = @_;
  $istop = 1 unless defined $istop;

  # base case: since preterminals are stored as one childless node, return
  # 0 or 1 depending on whether it's marked with a *
  if (scalar @{$node->{children}} == 0) {
#     print "BASE: HEIGHT($node->{label},$istop) = 1 (no kids)\n";
    if ($node->{label} =~ /^\*/) {
      return 1;
    } else {
      return 0;
    }
  }
  # base case: if the label isn't marked with *, then it's a nonterminal leaf,
  # so just return 0 for it -- EXCEPT if we're the top node, in which case
  # we pass through this case
  elsif ($node->{label} !~ /^\*/ and !$istop) {
#     print "BASE: HEIGHT($node->{label},$istop) = 0\n";
    return 0;
  }
  # else

  # recursive case
  my @heights = map { height($_,0) } @{$node->{children}};
  return 1 + max @heights;
}

# removes label annotations from a node and all its descendants 
sub clean {
  my $node = shift;
  $node->{label} =~ s/^\*//;
  map { clean($_) } @{$node->{children}};
}
