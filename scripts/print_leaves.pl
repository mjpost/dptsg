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
  lexicon => "/dev/null",
  thresh => 0,
  '*tags' => 0,
  words => 1,
);
process_params(\%PARAMS,\@ARGV,\%ENV);

my $lexicon = $PARAMS{thresh} > 0
	? read_lexicon($PARAMS{lexicon},$PARAMS{thresh})
	: undef;

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
