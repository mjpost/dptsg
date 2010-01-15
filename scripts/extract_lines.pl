#!/usr/bin/perl

# Reads a list of line numbers and extracts the corresponding
# lines from another file, writing them to stdout.  If the -v flag
# is given, then it prints out all lines *not* contained in the
# list of line numbers (analogous to grep's -v).
# 
# This script is useful in a pipeline where you first use grep to find
# line numbers matching a certain pattern, and want to prune those
# line numbers out of a parallel corpus contained in two separate
# files, e.g.,
#
# grep -n "UNDESIRABLE PATTERN" corp1 | awk -F: '{print $1}' > line_nos
# extract_lines.pl -l line_nos -f corp1 -v > corp1.trimmed
# extract_lines.pl -l line_nos -f corp2 -v > corp2.trimmed

use Getopt::Std;

my %options = ();
getopts('l:f:v', \%options);

my $lines;
open $lines, $options{l} || die "can't read line numbers from $options{l}";

open READ, $options{f} || die "can't read file $options{f}";

$sentno = 0;
for (;;) {
  $next = nextlineno($lines);
  # stop if there are no more lines to look for, unless we are printing
  # out reverse lines
  last unless ($next || $options{v});
  do {
    $line = <READ>;
    last unless $line;
    $sentno++;
    print $line if ($options{v} and $sentno != $next);
  } until $sentno == $next;
  print $line unless $options{v};
}

close READ;
close $lines;

sub nextlineno {
  my $fileh = shift;

  $lineno = <$fileh>;
  chomp $lineno;
  $lineno;
}
