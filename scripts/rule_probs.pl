#!/usr/bin/perl

# Combines rule counts and prints out their relative frequencies.
# Input can be either
# - a DOP-annotated corpus (node labels preceded by a * denote joined nodes),
#   one tree per line, e.g.,
#   (S (NP (NPB (NNP John)) (VP (VBZ is) (ADJP (JJ nice)))) (. .))    
# - rule counts in the format COUNT RULE, e.g.,
#   50910 (PP IN NPB)
#   48121 (, ,)
#   1140 (WHNP (WDT which))
#
# Example usage
# $ cat wsj.trees.02-21.clean | ./rule_probs.pl > data/pcfg_rules.prb
#   OR
# $ cat */counts | ./rule_probs.pl > data/pcfg_rules.prb
#
# If a line equalling "NEXT" is seen, all further rules read from the
# corpus will only be counted if they were not seen before the word
# NEXT

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
  '*counts' => 0,     # produce counts instead of probabilities
  rulethresh => 0,
  lexicon => "$basedir/data/lex.02-21",
  thresh => 0,     # min count of rule
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

my %COUNTS;

while (my $line = <>) {
  chomp($line);

  next if $line eq "(TOP)";

  $line =~ s/\s*$//;

  if ($line =~ /^\d/) {  # reading in counts
    my ($count,$label,@rest) = split ' ', $line;
    my $rule = join(" ", $label, @rest);
    $COUNTS{lhsof($rule)}{$rule} += $count;
  } else {               # reading in corpora
    my $tree = build_subtree($line,$lexicon);
    walk($tree,[\&count_rule]);
  }
}

my %LHS_COUNTS;
# sum lefthand sides
foreach my $lhs (keys %COUNTS) {
  map { $LHS_COUNTS{$lhs} += $_ } values %{$COUNTS{$lhs}};
}

foreach my $lhs (keys %COUNTS) {
  while (my ($key,$val) = each %{$COUNTS{$lhs}}) {
    if ($val < $PARAMS{rulethresh}) {
      delete $COUNTS{$lhs}->{$key};
      next;
    }
  }
}

foreach my $lhs (keys %COUNTS) {
  foreach my $rule (sort { $COUNTS{$lhs}{$b} <=> $COUNTS{$lhs}{$a} } keys %{$COUNTS{$lhs}}) {
    if ($PARAMS{counts}) {
      print "$COUNTS{$lhs}{$rule} $rule\n";
    } else {
      my $pr = ($COUNTS{$lhs}{$rule} / $LHS_COUNTS{$lhs});
      print "$pr $rule\n";
    }
  }
}

## SUBROUTINES #######################################################

sub count_rule {
  my $node = shift;
  my $lhs = $node->{label};

  if ($lhs !~ /^\*/ and @{$node->{children}}) {
    my $rule = ruleof($node);
    $COUNTS{$lhs}{$rule}++;
  }
}

