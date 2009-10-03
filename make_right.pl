#!/usr/bin/perl
# 2009-09-22 Matt Post

# Right-binarizes each node of a parse tree, naming intermediate nodes
# xbar style.

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use tree;

while (my $line = <>) {
  chomp($line);
  my $subtree = build_subtree($line);

  my $binarize = sub {
    my ($node) = @_;

    if ($node->{numkids} > 2) {
      # create the new node (a hash) with the proper name
      my $newnode = { 
        label => xbar($node->{label}),
        numkids => $node->{numkids} - 1
      };
      # replace the n-1 rightmost children of $node with $newnode,
      # assigning those children to $newnode
      @{$newnode->{children}}
        = splice @{$node->{children}}, 1, $node->{numkids}-1, $newnode;
    }
  };

  walk($subtree,[$binarize]);
  print build_subtree_oneline($subtree), $/;
}

sub xbar {
  my $label = shift;
  return ($label =~ /'$/) ? $label : "$label'";
}
