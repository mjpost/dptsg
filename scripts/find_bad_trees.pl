#!/usr/bin/perl

# finds good and bad trees, printing good ones to STDOUT and bad ones
# to STDERR

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my $bad = 0;
while (my $line = <>) {
  chomp $line;

  if ($line eq "(TOP)" or $line =~ /^\s*$/) {
    $bad++;
    print STDERR "$. $line\n";
    next;
  }

  my $tree = build_subtree($line);

  if (! defined $tree) {
    $bad++;
    print STDERR "$. $line\n";
    next;
  }

  print $line . $/;
}
