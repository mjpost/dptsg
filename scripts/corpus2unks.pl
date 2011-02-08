#!/usr/bin/perl

# takes a strings of words and a vocab and replaces unknown words
# with the appropriate UNK

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
  thresh => 2,
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

while (my $line = <>) {
  chomp $line;

  my @tokens = split(' ',$line);
  my @words  = map { signature($tokens[$_],($_ == 0)) } (0..$#tokens);

  print join(" ",@words) . $/;
}
