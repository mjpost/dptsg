#!/usr/bin/perl

# this script take a nasty derivation tree output by the berkeley
# parser and converts it into a parse tree suitable for scoring
# against the gold-standard parses

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

while (my $line = <>) {
  chomp($line);
  if ($line eq "(TOP)") {
    print "(TOP)\n";
    next;
  }

  my $tree = build_subtree($line);

  walk($tree,[\&clean]);

  print build_subtree_oneline($tree,1), $/;
}

sub clean {
  my ($node) = @_;

  $node->{label} =~ s/\^.*//;
  $node->{label} =~ s/_\d+$//;

  # remove everything after a hyphen, if that hyphen is not first
  $node->{label} =~ s/(.)-.+$/$1/;

  for (;;) {
    my $changed = 0;
    my $numkids = scalar @{$node->{children}};
    foreach my $kidno (0..$numkids-1) {
      my $kid = @{$node->{children}}[$kidno];
      if ($kid->{label} =~ /^\@/) {
        splice @{$node->{children}},$kidno,1,@{$kid->{children}};
        $changed = 1;
        last;
      }
    }
    last unless $changed;
  }
}
