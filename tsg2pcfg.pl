#!/usr/bin/perl

# Reads in a treebank file line by line and collapses all TSG rules to
# PCFG rules.  Rules with lexical items introduce a special
# preterminal that expands deterministically to that terminal

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

while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line,$lexicon);
  walk($subtree,[\&delete_nodes]);

  print build_subtree_oneline($subtree,1), $/;
}

sub delete_nodes {
  my ($node) = @_;

  # Find children that are TSG-internal nodes and delete them.  Since
  # deleting a child changes the list of the current node's children,
  # we need to start over each time we collapse a child.  This is
  # equivalent to do a depth-first collapsing of each TSG rule.
  my $changed = 0;
  do {
    $changed = 0;
    my $kidno = 0;
    my $numkids = scalar @{$node->{children}};
    for my $kidno (0..$numkids-1) {
      my $kid = @{$node->{children}}[$kidno];
      if ($kid->{label} =~ /^\*/) {
        $changed = 1;
        # if the internal node is a preterminal, rename it so that it
        # expands deterministically to its child; otherwise, delete it.
        if (is_preterminal($kid)) {
          my $grandkid = @{$kid->{children}}[0];
          $kid->{label} = "[$grandkid->{label}]";
        } else {
          splice @{$node->{children}}, $kidno, 1, @{$kid->{children}};
        }
      }
      $kidno++;
    }
  } while $changed;
}
