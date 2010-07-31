#!/usr/bin/perl

while ($line = <>) {
  chomp $line;

  my @matches = ($line =~ /_\)/g);
  print +(scalar @matches) . " $line\n";
}
