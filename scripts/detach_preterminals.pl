#!/usr/bin/perl

# detaches preterminal rules from larger subtrees

my $basedir;
BEGIN{
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;
use List::Util qw|sum|;

my %PARAMS = (
);
process_params(\%PARAMS,\@ARGV,\%ENV);

while (my $line = <>) {
  chomp($line);

  next if $line eq "(TOP)";

  my $tree = build_subtree($line);

  walk($tree,[\&detach]);

  print build_subtree_oneline($tree,1), $/;
}


## SUBROUTINES #######################################################

sub is_internal {
  my ($node) = @_;
  return ($node->{label} =~ /^\*/) ? 1 : 0;
}


sub detach {

  my ($node) = @_;
  
  if (is_internal($node) and is_preterminal($node)) {
    # print "$node->{label} -> ";
    $node->{label} =~ s/^\*//;
    # print "$node->{label}\n";
  }
}
