#!/usr/bin/perl

# builds the lexicon used by everything

use strict;
use warnings;
use TSG;

my %words;
while (my $line = <>) {
  chomp $line;

  if ($line eq "(TOP)" or $line =~ /^\s*$/) {
    print STDERR "[$.] WARNING: bad tree\n";
    next;
  }

  my $tree = build_subtree($line);

  if ($tree) {
    walk($tree,[\&count_words]);
    count_words($tree);
  }
}

my $i = 1;
map { print $i++ . " " . delex($_) . " $words{$_}\n"; } (sort {$words{$b} <=> $words{$a}} keys %words);
  
sub count_words {
  my($e) = @_;
  $words{$e->{label}}++
      if @{$e->{children}} == 0;
}
