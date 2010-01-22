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

my (%COUNTS,%UNKCOUNTS,%WORDCOUNTS,%TAGCOUNTS,%CLASSCOUNTS);

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

my $total_words = sum values %WORDCOUNTS;

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

my $k = 0.5;
foreach my $lhs (keys %COUNTS) {
  foreach my $rule (sort { $COUNTS{$lhs}{$b} <=> $COUNTS{$lhs}{$a} } keys %{$COUNTS{$lhs}}) {
    if ($PARAMS{counts}) {
      print "$COUNTS{$lhs}{$rule} $rule\n";
    } else {
      my $pr;
      if ($rule =~ /^\(\S+ _(\S+)_\)$/) {
        my $word = $1;

        my $class = classof($word);
        my $p_tag_word = ($COUNTS{$lhs}{$rule} + $k * tag_prob($lhs,classof($word))) / ($WORDCOUNTS{$word} + $k);
        if ($lhs eq "IN") {
          # print "* $COUNTS{$lhs}{$rule}\n";
          # print "* $k\n";
          # print "* tag_prob($lhs,$class) = " . tag_prob($lhs,classof($word)) . $/;
          # print "* WORDCOUNTS($word) $WORDCOUNTS{$word}\n";
        }
 
        # P(word|tag) = P(tag|word) * P(word) / P(tag)
        $pr = $p_tag_word * ($WORDCOUNTS{$word}/$total_words) / ($TAGCOUNTS{$lhs}/$total_words);
        
      } else {
        $pr = ($COUNTS{$lhs}{$rule} / $LHS_COUNTS{$lhs});
      }
      print "$pr $rule\n";
    }
  }
}

foreach my $tag (keys %UNKCOUNTS) {
  foreach my $class (keys %{$UNKCOUNTS{$tag}}) {
    # my $tag_prob = tag_prob($tag,$class);
    # my $pr = $tag_prob * ($TAGCOUNTS{$tag} / $total_words) / ($CLASSCOUNTS{$class} / $total_words);

    my $pr = ($UNKCOUNTS{$tag}{$class} / $total_words) / ($TAGCOUNTS{$tag} / $total_words);
    print "* $tag $class $UNKCOUNTS{$tag}{$class} $TAGCOUNTS{$tag} $total_words ($pr)\n";

    print "$pr ($tag _${class}_)\n";
  }
}

## SUBROUTINES #######################################################

sub count_rule {
  my $node = shift;
  my $lhs = $node->{label};

  if ($lhs !~ /^\*/ and @{$node->{children}}) {
    my $rule = ruleof($node);
    $COUNTS{$lhs}{$rule}++;

    if (is_preterminal($node)) {
      my $word = delex(@{$node->{children}}[0]->{label});
      # print "UNKCOUNTS($lhs," . classof($word) . ")\n";
      $UNKCOUNTS{$lhs}{classof($word)}++;
      $WORDCOUNTS{$word}++;
      $TAGCOUNTS{$lhs}++;
      $CLASSCOUNTS{classof($word)}++;
    }
  }
}

# computes Pr(class | tag)
sub tag_prob {
  my ($tag,$class) = @_;

  my $pr = $UNKCOUNTS{$tag}{$class} / $TAGCOUNTS{$tag};
  # print "TAG_PROB($tag,$class) = $pr\n";

  return $pr;
}
