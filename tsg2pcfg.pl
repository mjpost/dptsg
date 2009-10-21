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
process_params(\%PARAMS,\@ARGV,\%ENV);

my $lexicon = read_lexicon($PARAMS{lexicon});

my $SENTNO = 0;
while (my $line = <>) {
  chomp($line);

  $SENTNO++; 

  my $subtree = build_subtree($line,$lexicon);
  {
    # some subtrees have the top-level node (beneath TOP) marked as an
    # internal node.  remove this when flattening
    my $kid = @{$subtree->{children}}[0];
    $kid->{label} =~ s/^\*//;
  }
  walk($subtree,[\&delete_nodes]);
  print build_subtree_oneline($subtree,1), $/;
  walk($subtree,[\&verify_one_head]);
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

    # see if any kid has its head explicitly marked
    my $headpos = -1;
    for my $kidno (0..$numkids-1) {
      my $kid = @{$node->{children}}[$kidno];
      if (has_marked_head($kid)) {
        $headpos = $kidno;
        last;
      }
    }

    for my $kidno (0..$numkids-1) {
      my $kid = @{$node->{children}}[$kidno];
      if (is_internal($kid)) {
        $changed = 1;

        # case 1: if the child we are collapsing is not explicitly
        # marked as the head (and another child is), we need to remove
        # the head markings from all the children we are bringing up
        if ($headpos >= 0 and $headpos != $kidno) {
          map { $_->{label} =~ s/^(.*[^\\])\*$/$1/ } @{$kid->{children}};
        }

        # if the internal node is a preterminal, rename it so that it
        # expands deterministically to its child; otherwise, delete it.
        if (is_preterminal($kid)) {
          my $grandkid = @{$kid->{children}}[0];
          $kid->{label} = clean($kid->{label}) ."_". delex($grandkid->{label});
          $kid->{label} .= '*' if ($headpos >= 0 and $headpos == $kidno);
        } else {
          splice @{$node->{children}}, $kidno, 1, @{$kid->{children}};
        }

        last;
      }
      $kidno++;
    }
  } while $changed;
}

sub verify_one_head {
  my ($node) = @_;

  if (! is_preterminal($node) and @{$node->{children}}) {
    my $headcount = 0;
    my $numkids = @{$node->{children}};
    for my $kidno (0..$numkids-1) {
      my $kid = @{$node->{children}}[$kidno];
      $headcount++ if has_marked_head($kid);
    }

    die "* FATAL: sentence $SENTNO " . ruleof($node). " has $headcount heads\n"
        if ($headcount != 1);
  }
}

sub has_marked_head {
  my ($node) = @_;
  return ($node->{label} =~ /[^\\]\*$/) ? 1 : 0;
}

sub is_internal {
  my ($node) = @_;
  return ($node->{label} =~ /^\*/) ? 1 : 0;
}

sub clean {
  my ($label) = @_;
  $label =~ s/^\*//;
  $label =~ s/\*$//;
  return $label;
}
