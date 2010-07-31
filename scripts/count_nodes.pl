#!/usr/bin/perl

# counts the number of nodes in a subtree

while ($line = <>) {
  chomp $line;

  my @matches = ($line =~ /\(/g);
  print +(scalar @matches) . " $line\n";
}
