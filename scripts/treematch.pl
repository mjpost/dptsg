#!/usr/bin/perl
$|++;  # no output buffering

# Matches grammar fragments to trees on STDIN.  For each tree, prints
# all fragments that matched.
#
# Example usage:
# cat trees | treematch.pl -grammar GRAMMAR

BEGIN {
  push @INC, "$ENV{DPTSG}";
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  lexicon => "$ENV{HOME}/expts/lsa11/data/lex.02-21",
  thresh => 2,
  '*munge' => 0,  # remove spaces and colons from fragments if nonzero
  grammar => undef,
);

# process paramters and read in lexicon
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

die "need grammar" unless defined $PARAMS{grammar};

my %fragments;
open GRAMMAR, $PARAMS{grammar} or die;
while (my $line = <GRAMMAR>) {
  chomp($line);
  next if $line =~ /^#/;
  my (undef,$str) = split(' ', $line, 2);

  my $fragment = build_subtree($str);

  push @{$fragments{$fragment->{label}}}, $fragment;
}
close GRAMMAR;

while (my $line = <>) {
  chomp($line);

  my $tree = build_subtree($line,$lexicon);
  my @nodes = ($tree);

  my %matches;
  while (@nodes) {
	my $node = pop @nodes;
	next unless $node->{numkids} > 0;

	foreach my $fragment (@{$fragments{$node->{label}}}) {
	  if (findat($node, $fragment)) {
		$matches{maybe_munge(build_subtree_oneline($fragment))}++;
	  }
	}

	push @nodes, @{$node->{children}};
  }

  print join("\t", map { "$_:$matches{$_}" } (keys %matches));
  print $/;
}

sub findat {
  my ($tree, $fragment) = @_;

  # print "TREE: ".($tree->{label}||"UNDEF")." FRAGMENT: ".($fragment->{label}||"UNDEF")."\n";

  return 0 unless $tree->{label} eq $fragment->{label};
  return 1 if $fragment->{numkids} == 0;
  return 0 unless $tree->{numkids} == $fragment->{numkids};

  for my $i (0..$fragment->{numkids}-1) {
	if (! findat(@{$tree->{children}}[$i], @{$fragment->{children}}[$i])) {
	  return 0;
	}
  }
  
  return 1;
}

# maybe remove spaces and colons from fragments
sub maybe_munge {
  my ($key) = @_;
  if ($PARAMS{munge}) {
    $key =~ s/ /_/g;
    $key =~ s/:/_COLON_/g;
  }
  return $key;
}
