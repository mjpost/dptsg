#!/usr/bin/perl

# Extracts an implicit DOP grammar following Goodman (1996)


use strict;
use warnings;
use List::Util qw/reduce/;
use TSG;

my %PARAMS = (
);
# process paramters and read in lexicon
process_params(\%PARAMS,\@ARGV,\%ENV);

my $NODENUM = 0;
my %TOTALS;

# iterate over treebank trees
my @treebank;
while (my $line = <>) {
  chomp($line);

  my $tree = build_subtree($line);

  push(@treebank,$tree);

  walk_postorder($tree,[\&mark_and_count]);
}

sub mark_and_count {
  my ($tree) = @_;
  unless (@{$tree->{children}}) {
	$tree->{subtrees} = 0;
	return;
  }

  $tree->{number} = $NODENUM++;
  my $prod = 1;
  foreach my $kid (@{$tree->{children}}) {
	$prod *= ($kid->{subtrees} + 1);
  }
  $tree->{subtrees} = $prod;
  $TOTALS{$tree->{label}} += $tree->{subtrees};

  # print "$tree->{label}/$tree->{number} has $tree->{subtrees} subtrees\n";
}

