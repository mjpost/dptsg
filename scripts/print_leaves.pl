#!/usr/bin/perl

# prints frontier of tree

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
  thresh => 0,
  tags => 0,
  words => 1,
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

while (<>) {
  chomp;
  walk(build_subtree($_,$lexicon),[\&print_leaves]);
  print $/;
}

sub print_leaves {
  my($e) = @_;
  if ($PARAMS{tags} and is_preterminal($e)) {
    print $e->{label}, " ";
  } elsif ($PARAMS{words} and @{$e->{children}} == 0) {
    print delex($e->{label}), " ";
  }
}
