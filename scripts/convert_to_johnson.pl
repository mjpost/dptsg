#!/usr/bin/perl

# this script takes a grammar and converts it to a form useable by
# Mark Johnson's CKY parser

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my (%rules,%words);

print "0.00000000000000000001 TOP --> DUMMY\n";
while (my $line = <>) {
  chomp($line);

  my ($prob, @rest) = split(' ',$line);

  my $tree = build_subtree(join(" ",@rest));

  my $frontier = $tree->{frontier};
  my @rank = split(" ",$frontier);
  if (@rank > 1) {
    # grab all the words from the RHS of the rule to be added as rules later
    map {$words{$_} = 1} ($frontier =~ /_(\S+)_/g);
    # print "FOUND WORDS(", join(",",@words), ") for LHS($tree->{label})\n";

    $frontier =~ s/_(\S+)_/[$1]/g;
  }
  print "$prob $tree->{label} --> $frontier\n";
}

map { print "1.0 [$_] --> $_\n" } (keys %words);
