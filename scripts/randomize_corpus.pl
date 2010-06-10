#!/usr/bin/perl

# randomly sets each node as internal or external

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  prob => 0.5,  # probability of node being internal
);
process_params(\%PARAMS,\@ARGV,\%ENV);

while (<>) {
  chomp;
  my $subtree = build_subtree($_);

  map { walk($_, [\&randomize]) } @{$subtree->{children}};
  print build_subtree_oneline($subtree) . $/;
}

sub randomize {
  my ($node) = @_;

  return if is_terminal($node);

  $node->{label} =~ s/^\*//;
  if (rand() > $PARAMS{prob}) {
    $node->{label} = "*" . $node->{label};
  }
}
