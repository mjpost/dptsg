#!/usr/bin/perl

# prints frontier of tree

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
  thresh => 2,
  tags => 0,
  words => 1,
);
process_params(\%PARAMS,\@ARGV,\%ENV);

my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

while (<>) {
  chomp;
  my $tree = build_subtree($_,$lexicon);
  mark_spans($tree);
  print_subtree($tree);
  print $/;
}

sub print_subtree {
  my($e,$dist) = @_;
  $dist = 0 unless defined $dist;
  print " " x $dist, $e->{label};
  if (is_preterminal($e)) {
    my $kid = @{$e->{children}}[0];
    print " $kid->{label} ($e->{i},$e->{j})\n";
  } else {
    print " ($e->{i},$e->{j})\n";
    map { print_subtree($_,$dist + 2) } @{$e->{children}};
  }
}

