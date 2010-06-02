#!/usr/bin/perl

# converts a berkeley grammar (files <name>.txt.{grammar,lexicon}) to
# a format usable by our parser
#
# - removes self-rewrites (e.g., ROOT-0 -> ROOT-0)
# - converts lhs of ROOT-0 to TOP
# - ignores rules with probability 0
# - convert _\d+$ in nonterminals to -\d+$ (underscores to hyphens)

use strict;
use warnings;

my $name = shift;

die "need prefix" unless $name;

open GRAMMAR, "${name}.grammar" or die "can't open grammar";
while (my $line = <GRAMMAR>) {
  chomp($line);

  my ($lhs,undef,@rhs) = split(" ",$line);
  my $prob = pop @rhs;
  next unless $prob > 0.0;

  $lhs = todash($lhs);

  if (@rhs == 1) { # unary rule
    my ($rhsl) = @rhs;
    $rhsl = todash($rhsl);
    # skip rewrites to self (why are these present?)
    next if ($lhs eq $rhsl);
    # rename the root
    $lhs = "TOP" if $lhs eq "ROOT-0";
    print "$prob ($lhs $rhsl)\n";
  } elsif (@rhs == 2) { # binary rule
    my ($rhsl,$rhsr) = @rhs;
    $rhsl = todash($rhsl);
    $rhsr = todash($rhsr);
    print "$prob ($lhs $rhsl $rhsr)\n";
  } else {
    die "bad rule!";
  }
}
close GRAMMAR;

open LEXICON, "${name}.lexicon" or die "can't open lexicon";
while (my $line = <LEXICON>) {
  chomp($line);

  my ($tag,$word,@probs) = split(" ",$line);

  @probs = map {
    s/\[//;
    s/\]//;
    s/,//;
    $_;
  } @probs;

  for my $i (0..$#probs) {
    my $prob = $probs[$i];
    # print STDERR "$prob $tag $word\n" if ($word eq "Shipbuilding");
    print "$prob (${tag}-$i _${word}_)\n"
        if ($prob > 0.0);
  }
}
close LEXICON;

sub todash {
  my $arg = shift;
  $arg =~ s/\^g//;
  $arg =~ s/_(\d+)$/-$1/;
  return $arg;
}
