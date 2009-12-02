#!/usr/bin/perl

# preprocesses treebank file

use strict;
use warnings;

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use TSG;

my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
  thresh => 1,
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

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
