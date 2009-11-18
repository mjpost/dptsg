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
  thresh => 2,
  tags => 0,
  words => 1,
);
process_params(\%PARAMS,\@ARGV,\%ENV);

while (<>) {
  chomp;
  walk(build_subtree($_),[\&print_leaves]);
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
