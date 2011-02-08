#!/usr/bin/perl
# 2010-01-16 Matt Post

# takes a grammar and converts it to a format useable by our decoder

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

# parameters (via environment variables and command-line params)
my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
  thresh => 2,
);
process_params(\%PARAMS,\@ARGV,\%ENV);

my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

my %temprules;
my %words;
my %nts;
while (<>) {
  chomp;
  my ($prob,@rule) = split;
  my $rule = join(" ",@rule);
  my $tree = build_subtree($rule,$lexicon);

  my $islex = ($rule =~ /_/ ? 1 : 0);

  # my $newrule = "($tree->{label} $tree->{frontier})";
  # my $newtree = build_subtree($newrule,$lexicon);

  # record the probability of each (lhs,frontier) pair, used later for
  # greedy binarization
  my $label = $tree->{label};
  my $frontier = $tree->{frontier};
  $temprules{"$label $frontier"} = $prob;

  $nts{$label} = 1;

  foreach my $rhs (split(" ", $tree->{frontier})) {
    if ($rhs =~ /_/) {  # lexical item
      $nts{$rhs} = 1;
      $rhs =~ s/_//g;
      $words{$rhs} = 1;
    }
  }
}

my ($rules,$binmap,$rulemap) = binarize_grammar(\%temprules,"all");

# do weight pushing
push_weights($binmap,$rules,$rulemap,"max");



while (my ($rhs,$hash) = each %$rules) {
  while (my ($lhs,$prob) = each %$hash) {
    $nts{$lhs} = 1;
    my (@rhs) = split($;,$rhs);
    if (@rhs == 2) {
      # arbitrarily mark the left guy as head
      print log($prob) . " $lhs -> 2 *$rhs[0] $rhs[1]\n";
    } else {
      print log($prob) . " $lhs -> 1 *$rhs[0]\n";
    }
  }
}

map { print "0 _${_}_ >> $_\n" } keys %words;

map { print STDERR $_, $/ } keys %nts;
