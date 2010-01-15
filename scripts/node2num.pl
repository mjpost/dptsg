#!/usr/bin/perl

# changes node names to numbers

use strict;
use warnings;

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use TSG;

my %PARAMS = (
);
process_params(\%PARAMS,\@ARGV,\%ENV);

my %ids;

while (my $line = <>) {
  chomp $line;

  my $subtree = build_subtree($line);
  walk($subtree,[\&rename_node]);
  print build_subtree_oneline($subtree), $/;
}

sub rename_node {
  my ($node) = @_;

  if (@{$node->{children}}) {
    if (! exists $ids{$node->{label}}) {
      $ids{$node->{label}} = scalar keys %ids;
    }
    $node->{label} = $ids{$node->{label}};
  }
}
