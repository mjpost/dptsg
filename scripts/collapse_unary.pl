#!/usr/bin/perl

use strict;
use warnings;
use TSG;

while (my $line = <>) {
  chomp($line);

  my $tree = build_subtree($line);

  walk_postorder($tree, [\&collapse_unary]);

  print build_subtree_oneline($tree) . $/;
}

sub collapse_unary {
  my ($tree) = @_;
  
  return if $tree->{label} eq "TOP";
  return if is_preterminal($tree);

  my $numkids = scalar @{$tree->{children}};
  if ($numkids == 1) {
	my $kid = @{$tree->{children}}[0];

	  # my $numgrandkids = scalar @{$kid->{children}};
	if (! is_preterminal($kid)) { # and $numgrandkids > 0) {
	  $tree->{children} = $kid->{children};
	  $tree->{label} .= "_$kid->{label}";
	}
  }
}
