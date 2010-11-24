#!/usr/bin/perl

# binarizes a corpus of trees with left-binarization, introducing @
# symbols at binarized nodes

my $basedir;
BEGIN{
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;
use List::Util qw|sum|;

my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
  thresh => 1,        # if extracting from a tree, min count for lex items
  dir    => 'left',
);
process_params(\%PARAMS,\@ARGV,\%ENV);

while (my $line = <>) {
  chomp($line);

  my $tree = build_subtree($line);
  binarize_subtree({ node=>$tree, dir=>$PARAMS{dir}, collapse=>"@", unique=>0 });

  print build_subtree_oneline($tree,1), $/;
}
