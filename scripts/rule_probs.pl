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
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;
use List::Util qw|sum|;

my %PARAMS = (
  '*counts' => 0,     # produce counts instead of probabilities
  rulethresh => 1,    # rules with lower counts are thrown out
  lexicon => "$basedir/data/lex.02-21",
  thresh => 1,        # if extracting from a tree, min count for lex items
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

my (%COUNTS,%UNKCOUNTS); #,%WORDCOUNTS,%TAGCOUNTS,%CLASSCOUNTS);

while (my $line = <>) {
  chomp($line);

  next if $line eq "(TOP)";

  $line =~ s/\s*$//;

  if ($line =~ /^\d+(\.\d+)?\s/) {  # reading in counts
		# replace lexical items with their signatures
		$line =~ s/_(\S+?)_/"_" . signature($1) . "_"/eg;

    my ($count,$label,@rest) = split ' ', $line;
    my $rule = join(" ", $label, @rest);

    my $lhs = $label;  $lhs =~ s/^\(//;

    increment_counts($lhs,$rule,$count);

  } else {               # reading in corpora
    my $tree = build_subtree($line,$lexicon);
    walk($tree,[\&count_rule]);
  }
}

# my $total_words = sum values %WORDCOUNTS;

# sum lefthand sides for each nonterminal
my %LHS_COUNTS;
foreach my $lhs (keys %COUNTS) {
  map { $LHS_COUNTS{$lhs} += $_ } values %{$COUNTS{$lhs}};
}

# delete rules whose count is below the rule threshold; note that this
# occurs AFTER summing for each lefthand side
foreach my $lhs (keys %COUNTS) {
  while (my ($key,$val) = each %{$COUNTS{$lhs}}) {
    if ($val < $PARAMS{rulethresh}) {
      delete $COUNTS{$lhs}->{$key};
      next;
    }
  }
}

# print out the counts or the rule probabiliteis
foreach my $lhs (keys %COUNTS) {
  foreach my $rule (sort { $COUNTS{$lhs}{$b} <=> $COUNTS{$lhs}{$a} } keys %{$COUNTS{$lhs}}) {

    if ($PARAMS{counts}) {
      # print the rule count 
      print "$COUNTS{$lhs}{$rule} $rule\n";
    } else {
      # print the rule probability
      my $pr = $COUNTS{$lhs}{$rule} / $LHS_COUNTS{$lhs};
      print "$pr $rule\n";
    }
  }
}

## SUBROUTINES #######################################################

sub count_rule {
  my ($node) = @_;
  my $lhs = $node->{label};

  # if the current node is the head of a rule, increment relevant
  # counts
  if ($lhs !~ /^\*/ and @{$node->{children}}) {
    increment_counts($lhs,ruleof($node));
  }
}

sub increment_counts {
  my ($lhs,$rule,$count) = @_;
  $count = 1 unless defined $count;
  
  # the count of the rule itself
  $COUNTS{$lhs}{$rule} += $count;

  # for preterminal rules, we also maintain counts for the
  # distribution over unknown word tokens
  if ($rule =~ /^\(\S+ _(\S+)_\)$/) {
    my $word = delex($1);
    # print "UNKCOUNTS($lhs," . classof($word) . ")\n";
    $UNKCOUNTS{$lhs}{classof($word)} += $count;
    # $WORDCOUNTS{$word} += $count;
    # $TAGCOUNTS{$lhs} += $count;
    # $CLASSCOUNTS{classof($word)} += $count;
  }
}
