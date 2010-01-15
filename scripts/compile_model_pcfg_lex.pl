#!/usr/bin/perl

# Reads in a corpus of PCFG derivations (use tsg2pcfg.pl to convert
# TSG derivations to PCFG ones first) and computes the statistics
# Pr(head word) and Pr(rule | head word)

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
# use Devel::Peek qw/Dump/;
use List::Util qw|reduce max min sum|;
use TSG;

my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
);
# check for environment variables overriding defaults
foreach my $key (keys %PARAMS) {
  if (exists $ENV{$key}) {
    $PARAMS{$key} = $ENV{$key};
    print STDERR "* $key = $PARAMS{$key} [env]\n";
  }
}
# process command-line arguments
while (@ARGV) {
  my $arg = shift;

  die "invalid option '$arg'" unless $arg =~ /^-/;

  $arg =~ s/^-//g;

  if (exists $PARAMS{$arg}) {
    $PARAMS{$arg} = shift @ARGV;
    print STDERR "* $arg = $PARAMS{$arg} [cmdline]\n";
  } else {
    die "no such option '$arg'";
  }
}

my $lexicon = read_lexicon($PARAMS{lexicon});

my %expansions;
while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line);
  $expansions{TOP}{TOP}{ruleof($subtree)}++;
  walk($subtree,[\&count_events]);
}

foreach my $parent (keys %expansions) {
  foreach my $lhs (keys %{$expansions{$parent}}) {
    my $sum = sum(values %{$expansions{$parent}{$lhs}});
    my $num = scalar keys %{$expansions{$parent}{$lhs}};
    print "$parent $lhs $num\n";
    while (my ($rule,$count) = each %{$expansions{$parent}{$lhs}}) {
      my $prob = $count / $sum;
      print "  $prob $rule\n";
    }
  }
}

sub count_events {
  my ($node) = @_;

  unless (is_preterminal($node)) {
    my $lhs = $node->{label};
    foreach my $kid (@{$node->{children}}) {
      my $kidrule = ruleof($kid);
      my $kidlhs = $kid->{label};
      $expansions{$lhs}{$kidlhs}{$kidrule}++;
      # print "($lhs)($kidlhs)($kidrule)\n";
    }
  }
}
