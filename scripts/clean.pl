#!/usr/bin/perl

# preprocesses treebank file

use strict;
use warnings;

use TSG;

my %PARAMS = (
  thresh => 0,
);

while (my $line = <>) {
  chomp $line;

  # build subtree
  my $subtree = build_subtree($line);
  # remove annotations
  walk($subtree,[\&scrub_node]);
  # prune nodes
  prune($subtree);
  # output
  print build_subtree_oneline($subtree,1), $/;
}
