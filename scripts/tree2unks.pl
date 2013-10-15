#!/usr/bin/perl

# Uses a lexicon to convert tree leaves to UNKS. Takes the trees in input (PTB format,
# one per line) and uses the passed-in lexicon to convert leaves falling below the
# threshold to signatures, writing the results on STDOUT.
#
# Usage:
# 
#   cat file.ptb | tree2unks.pl -lexicon LEX -thresh THRESH > file-unks.ptb
#
# where file.ptb is a file with trees one per line in PTB format, LEX is a lexicon
# with lines of the form "ID WORD COUNT", and THRESH is a threshold. Words appearing
# fewer than THRESH times will be signaturized.

use strict;
use warnings;
use TSG;

my %PARAMS = (
  lexicon => "$ENV{DPTSG}/data/lex.02-21",
  thresh => 2,
);

# process paramters and read in lexicon
process_params(\%PARAMS,\@ARGV,\%ENV);

my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line,$lexicon);

  print print_tree($subtree) . $/;
}

sub print_tree {
  my ($node) = @_;

  my $str;
  my $numkids = scalar @{$node->{children}};
  if ($numkids) {
    $str = "($node->{label}";
    # annotate heads
    map { $str .= " " . print_tree($_) } @{$node->{children}};
    $str .= ")";
  } else {
    $str = delex($node->{label});
  }

  return $str;
}
