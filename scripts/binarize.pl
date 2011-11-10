#!/usr/bin/perl

# binarizes a corpus of trees with left-binarization, introducing @
# symbols at binarized nodes

my $basedir;
BEGIN{
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;
use List::Util qw|sum|;

my %PARAMS = (
  lexicon  => "$basedir/data/lex.02-21",
  collapse => "@",
  dir      => 'left',
  unique   => 0,
);
process_params(\%PARAMS,\@ARGV,\%ENV);

while (my $line = <>) {
  chomp($line);

  my $tree = build_subtree($line);
  binarize_subtree({ node=>$tree, dir=>$PARAMS{dir}, collapse=>$PARAMS{collapse}, unique=>$PARAMS{unique} });

  print build_subtree_oneline($tree,1), $/;
}
