#!/usr/bin/perl

# takes a set of parses and gold standard parses, and outputs a new tree
# where correct nodes from the parses are annotated with a '*'.  This facilitates passes through the Berkeley grammar tree drawer, since it will box nodes that are NOT starred.

my $basedir;
BEGIN{
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;
use List::Util qw|sum|;

if (@ARGV != 1) {
  print "Usage: mark_gold_constituents.pl GOLD < PARSES\n";
  exit;
}
my $goldfile = shift;

my %SPANS;

open GOLD, $goldfile or die "can't read gold file $goldfile";
while (my $line = <GOLD>) {
  chomp($line);
  my $goldtree = build_subtree($line);
  mark_spans($goldtree);

  %SPANS = ();
  walk($goldtree,[\&extract_spans]);

  chomp($line = <>);
  my $tree = build_subtree($line);
  mark_spans($tree);

  walk($tree,[\&mark_good]);

  print build_subtree_oneline($tree,1), $/;
}

# print "PRECISION: $matches / $total = " . ($matches / $total) . $/;

sub mark_good {
  my ($node) = @_;
  my $lhs = $node->{label};

  # if the current node is the root of a rule, count it
  $node->{label} = "*$node->{label}" if $SPANS{"$lhs $node->{i} $node->{j}"};
}

sub extract_spans {
  my ($node) = @_;

  if ($node->{children}) {
    # print "GOLD($node->{label} $node->{i} $node->{j})\n";
    $SPANS{"$node->{label} $node->{i} $node->{j}"} = 1;
  }
}

sub coarse {
  my ($label) = @_;

  $label =~ s/-.*$//;
  return $label;
}
