#!/usr/bin/perl

# builds the lexicon used by everything

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %words;
while (my $line = <>) {
  chomp $line;
  my $tree = build_subtree($line);

  walk($tree,[\&count_words]);
  count_words($tree);
}

my $i = 1;
map { print $i++ . " " . delex($_) . " $words{$_}\n"; } (sort {$words{$b} <=> $words{$a}} keys %words);
  
sub count_words {
  my($e) = @_;
  $words{$e->{label}}++
      if @{$e->{children}} == 0;
}
