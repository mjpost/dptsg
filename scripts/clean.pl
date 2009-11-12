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
  thresh => 2,
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

while (my $line = <>) {
  chomp $line;

  my $subtree = prune(build_subtree($line,$lexicon));
  walk($subtree,[\&scrub_node]);
  print build_subtree_oneline($subtree,1), $/;
}
