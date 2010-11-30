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

  if ($line =~ /^\d+ /) {  # reading in counts
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

# compute the mass reserved for unknown words for each preterminal.
# that mass is the percentage (by token) of words with count 1
my %mass_for_unseen;
foreach my $tag (keys %UNKCOUNTS) {
  my $sum = 0;
  while (my ($rule,$count) = each %{$UNKCOUNTS{$tag}}) {
    $sum++ if $count == 1;
  }
  die "Zero count for tag $tag!" unless $LHS_COUNTS{$tag};
  $mass_for_unseen{$tag} = $sum / $LHS_COUNTS{$tag};
}  

# print out the counts or the rule probabiliteis
foreach my $lhs (keys %COUNTS) {
  foreach my $rule (sort { $COUNTS{$lhs}{$b} <=> $COUNTS{$lhs}{$a} } keys %{$COUNTS{$lhs}}) {

    if ($PARAMS{counts}) {
      # print the rule count 
      print "$COUNTS{$lhs}{$rule} $rule\n";
    } else {
      # print the rule probability
      my $pr;
      if ($rule =~ /^\(\S+ _(\S+)_\)$/) {
        my $word = $1;

        # the probability for preterminals has to make space for
        # unknown word rewrites
        $pr = (1.0 - $mass_for_unseen{$lhs}) * ($COUNTS{$lhs}{$rule} / $LHS_COUNTS{$lhs});
      } else {
        $pr = ($COUNTS{$lhs}{$rule} / $LHS_COUNTS{$lhs});
      }
      print "$pr $rule\n";
    }
  }
}

# now print the distribution over unknown word tokens, but only if we
# are outputting a probability distribution instead of counts
unless ($PARAMS{counts}) {
  foreach my $tag (keys %UNKCOUNTS) {
    foreach my $class (keys %{$UNKCOUNTS{$tag}}) {
      my $pr = $mass_for_unseen{$tag} * $UNKCOUNTS{$tag}{$class} / $LHS_COUNTS{$tag};
      print "$pr ($tag _${class}_)\n" if $pr > 0;
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
