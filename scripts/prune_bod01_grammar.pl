#!/usr/bin/perl

# takes a BOD01 extracted grammar (the output of extract_bod01_grammar.pl)
# and prunes it by various criteria

use strict;
use warnings;
use List::Util qw|max|;

my $skipped = 0;
while (my $line = <>) {
  chomp $line;

  my ($height,$count,$rulestr) = split(' ',$line,3);

  my @matches = ($line =~ /_/g);
  my $numterms = @matches / 2;

  if ($numterms > 12 || ($numterms == 0 && $height > 6)) {
    $skipped++;
  } else {
    print $line . $/;
  }
}

print STDERR "skipped $skipped types\n";
