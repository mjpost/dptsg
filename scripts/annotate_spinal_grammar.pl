#!/usr/bin/perl

# Annotates the spinal grammar by splitting rules where head changes
# occur.  For example, if the tree fragment
#    (A * (B *) *)
# is encountered, and head(A) != head(B), we mark the node with a * so
# the rule can be extracted.

use strict;
use warnings;

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use TSG;

my (%counts,%lhs_counts);

while (my $line = <>) {
  chomp $line;

  my $tree = build_subtree($line);
  walk_postorder($tree,[\&mark_heads,\&annotate]);

  @{$tree->{children}}[0]->{label} =~ s/^\*//; # undo top-level merge
  
  print build_subtree_oneline($tree,1), $/;
}

## subroutines #######################################################

sub annotate {
  my $tree = shift;

  if (@{$tree->{children}} && ! is_preterminal($tree)) {
    my $headkid = @{$tree->{children}}[$tree->{hpos}];
    $headkid->{label} =~ s/^/*/o;
  }
}
