#!/usr/bin/perl

# takes a tree (optionally with TSG fragment markers) and outputs a
# latex qtree-style tree

$tsg = shift @ARGV;

while (my $line = <>) {
  chomp($line);

  if ($tsg) {
    $line =~ s/\(([^*].*?) /(\\node[draw]{$1}; /g;
  }

  $line =~ s/\(/[./g;
  $line =~ s/\)/ ]/g;
  $line =~ s/\*//g;
  $line =~ s/_//g;

  print "\\Tree $line\n";
}
