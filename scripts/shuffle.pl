#!/usr/bin/perl

# use
# perl -MList::Util=shuffle -alne 'print "@{[shuffle @F]}"'
# from the command line if you don't care about the seed

use List::Util qw/shuffle/;

my $seed = time ^ $$;

srand($seed);

print STDERR "seed $seed\n";

while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line);

  my @tokens = split(' ', $line);

  print join(" ",shuffle(@tokens)), $/;
}
