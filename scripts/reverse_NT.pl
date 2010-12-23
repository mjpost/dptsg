#!/usr/bin/perl

# prints frontier of tree

my $basedir;
BEGIN {
  $basedir = "$ENV{DPTSG}";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  nt => "NP"    # which nonterminal to reverse the children of
);
process_params(\%PARAMS,\@ARGV,\%ENV);

while (<>) {
  chomp;
  my $tree = build_subtree($_);
  walk($tree,[\&reverse_NT,\&print_leaves]);
  print $/;
}

sub reverse_NT {
  my ($node) = @_;

  if ($node->{label} eq $PARAMS{nt}) {
    my @children = reverse @{$node->{children}};
    $node->{children} = \@children;
  }
}

sub print_leaves {
  my($e) = @_;
  if (@{$e->{children}} == 0) {
    print delex($e->{label}), " ";
  }
}
