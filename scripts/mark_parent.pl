#!/usr/bin/perl

# Annotates each node in the corpus with its parent.

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  mark => '-'
);
# process paramters and read in lexicon
process_params(\%PARAMS,\@ARGV,\%ENV);

while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line);
  walk($subtree,[\&mark_parent]);
  
  walk_postorder($subtree,[\&annotate_with_parent]);

  print build_subtree_oneline($subtree,1), $/;
}

sub annotate_with_parent {
  my ($node) = @_;

  # post-order depth-first traversal
  if (! is_terminal($node) and $node->{parent}) {
    my $label = $node->{label};
    if ($label =~ /^\*/) {
      $node->{label} =~ s/^\*//;
      $node->{label} = "*$node->{label}$PARAMS{mark}$node->{parent}->{label}";
    } else {
      $node->{label} = "$node->{label}$PARAMS{mark}$node->{parent}->{label}";
    }
  }
}
