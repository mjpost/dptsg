#!/usr/bin/perl

# this script takes a grammar and converts it to a form useable by
# Mark Johnson's CKY parser

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my (%rules,%map);

print "0.00000000000000000001 TOP --> DUMMY\n";
while (my $line = <>) {
  chomp($line);

  my ($prob, @rest) = split(' ',$line);

  my $line = join(" ",@rest);
  my $tree = build_subtree($line);

  my $frontier = $tree->{frontier};
  my $rule = "$tree->{label} --> $frontier";

  if (! exists $rules{$rule} || $prob > $rules{$rule}) {
    $rules{$rule} = $prob;

    if ($tree->{depth} > 1) {
      $map{$rule}   = $line;
    }
  }
}

while (my ($rule,$prob) = each %rules) {
  my @tokens = split(' --> ',$rule);
  my $flatrule = "(" . join(" ",@tokens) . ")";
  my $tree = build_subtree($flatrule);
  binarize_subtree({ node=>$tree, dir=>"terminal", collapse=>"none", unique=>1 });

  my @rules;
  extract_subtrees($tree,\@rules);

  print "$prob " . parens_to_arrow($rules[0]) . $/;
  shift @rules;
  foreach my $rule (@rules) {
	print "1.0 " . parens_to_arrow($rule) . $/;
  }

  # print "$prob $rule\n";
}

while (my ($flat,$full) = each %map) {
  print STDERR "$flat ||| $full\n";
}

sub parens_to_arrow {
  my ($parens) = @_;

  $parens =~ s/\(|\)//g;
  my @tokens = split(' ',$parens);

  return $tokens[0] . " --> " . join(" ",@tokens[1..$#tokens]);
}
