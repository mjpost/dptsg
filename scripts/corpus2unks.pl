#!/usr/bin/perl

# takes a strings of words and a vocab and replaces unknown words
# with the appropriate UNK

use strict;
use warnings;
use TSG;

my %PARAMS = (
  lexicon => "$ENV{DPTSG}/data/lex.02-21",
  thresh => 2,
  '*no-initc' => 0,
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

while (my $line = <>) {
  chomp $line;

  my @tokens = split(' ',$line);
  my @words  = map { signature($tokens[$_],($_ == 0)) } (0..$#tokens);

  if ($PARAMS{'no-initc'}) {
	map {
	  $_ =~ s/-INITC|-KNOWNLC//g;
	} @words;
  }

  print join(" ",@words) . $/;
}
