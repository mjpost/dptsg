#!/usr/bin/perl

# reads in a tree and prints it out

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  thresh => 2,
  lexicon => "$basedir/data/lex.02-21",
);
process_params(\%PARAMS,\@ARGV,\%ENV);

my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line,$lexicon);

  print build_subtree_oneline($subtree,1), $/;
}

