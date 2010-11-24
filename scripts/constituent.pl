#!/usr/bin/perl

# takes a set of parses and gold standard parses, and outputs the
# percentage of time each rule was used correctly (i.e., that it
# matched the gold standard parse)

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
  lexicon => "$basedir/data/lex.02-21",
  thresh => 1,        # if extracting from a tree, min count for lex items
  counts => undef,    # file of counts of each subtree in training data
  gold => undef,
);
process_params(\%PARAMS,\@ARGV,\%ENV);

my %SPANS;

open GOLD, $PARAMS{gold} or die "can't read gold file $PARAMS{gold}";
while (my $line = <GOLD>) {
  chomp($line);
  my $goldtree = build_subtree($line);
  mark_spans($goldtree);

  %SPANS = ();
  walk($goldtree,[\&extract_spans]);

  chomp($line = <>);
  my $tree = build_subtree($line);
  mark_spans($tree);

  walk($tree,[\&count_rule]);
  walk($tree,[\&count_good_rule]);
}

# read in counts of rules in training data
my %COUNTS;
if ($PARAMS{counts}) {
  open COUNTS, $PARAMS{counts} or die;
  while (my $line = <COUNTS>) {
    chomp($line);
    my ($count,@rule) = split(' ',$line);
    $COUNTS{join(" ",@rule)} = $count;
  }
  close COUNTS;
}

my (%RULES,%GOODRULES);

my $total = 0;
my $matches = 0;

if ($PARAMS{counts}) {
  print "pct\tgood\tcount\ttcount\tlabel\tcoarse\trule\n";
} else {
  print "pct\tgood\tcount\tlabel\tcoarse\trule\n";
}

while (my ($rule,$count) = each %RULES) {
  $total += $count;

  my $good = $GOODRULES{$rule} || 0;
  $matches += $good;

  my $label = $rule;     $label =~ s/^\((\S+).*$/$1/;
  my $coarse = $label;   $coarse =~ s/-.*//;

  my $pct = 1.0 * $good / $count;
  my $tcount = "";
  if ($PARAMS{counts}) {
    $tcount = "\t$COUNTS{$rule}";
  }
  print "$pct\t$good\t$count$tcount\t$label\t$coarse\t$rule\n";
}

# print "PRECISION: $matches / $total = " . ($matches / $total) . $/;

sub count_rule {
  my ($node) = @_;
  my $lhs = $node->{label};

  # if the current node is the root of a rule, count it
  if ($lhs !~ /^\*/ and @{$node->{children}} && ! is_preterminal($node)) {
    $RULES{ruleof($node)}++;
  }
}

sub count_good_rule {
  my ($node) = @_;
  my $lhs = $node->{label};

  # if the current node is the root of a rule, count it
  if ($lhs !~ /^\*/ and @{$node->{children}} && ! is_preterminal($node)) {
    my $rule = goodruleof($node);
    # print "* GOODRULE($rule)\n";
    $GOODRULES{$rule}++ if $rule;
  }
}

sub extract_spans {
  my ($node) = @_;

  if ($node->{children}) {
    # print "GOLD($node->{label} $node->{i} $node->{j})\n";
    $SPANS{"$node->{label} $node->{i} $node->{j}"} = 1;
  }
}

sub coarse {
  my ($label) = @_;

  $label =~ s/-.*$//;
  return $label;
}

# builds the rule
sub goodruleof {
  my ($node) = @_;

  my $label = $node->{label};
  $label =~ s/^\*//;

  my $coarselabel = coarse($label);
  if (! $SPANS{"$coarselabel $node->{i} $node->{j}"}) {
    # print "COULDN'T FIND SPAN($node->{label}/$coarselabel $node->{i} $node->{j})\n";
    return undef;
  } else {
    # print "FOUND SPAN($node->{label}/$coarselabel $node->{i} $node->{j})\n";
  }

  my $str;
  if (scalar @{$node->{children}}) {
    $str = "($label";
    foreach my $kid (@{$node->{children}}) {
      if ($kid->{label} =~ /^\*/) {
        my $recurse = goodruleof($kid);
        return undef unless $recurse;
        $str .= " " . $recurse;
      } else {
        my $lab = $kid->{label};
        $lab =~ s/^\*//;
        $str .= " " . $lab;
      }
    }
    $str .= ")";
  } else {
    $str .= $node->{label};
  }
  return $str;
}
