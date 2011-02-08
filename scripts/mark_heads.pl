#!/usr/bin/perl

# reads in a treebank and marks the head child of each node useful as
# a preprocessing step when squashing a TSG down to a PCFG, if you
# want to retain head information

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
# use Devel::Peek qw/Dump/;
use List::Util qw|reduce max min sum|;
use TSG;

my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
);
process_params(\%PARAMS,\@ARGV,\%ENV);

my $head_symbol = '*';

my $lexicon = read_lexicon($PARAMS{lexicon});

while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line,$lexicon);
  walk($subtree,[\&mark_head]);

  print build_subtree_oneline($subtree,1), $/;
}

sub mark_head {
  my ($node) = @_;

  if ($node->{numkids} and ! is_preterminal($node)) {
    my $hpos = $node->{hpos};
    if ($hpos >= 0) {
      my $head = @{$node->{children}}[$hpos];
      $head->{label} .= $head_symbol;
    }
  }
}
