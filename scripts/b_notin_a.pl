#!/usr/bin/perl

# takes two lists A and B of (count,item) pairs, and tells you the
# percentage of items found in list B that were not in list A (by type
# and by token)

use strict;
use warnings;

my $file1 = shift;
my $file2 = shift;

my $hash1 = read_file($file1);
my $hash2 = read_file($file2);

my $types  = 0;
my $newtypes = 0;
my $tokens = 0;
my $newtokens = 0;

while (my ($item,$count) = each %$hash2) {
  $types  += 1;
  $tokens += $count;
  if (! exists $hash1->{$item}) {
    $newtypes++;
    $newtokens += $count;
  }
}

printf("$newtypes / $types (%.2f%%) by type, $newtokens / $tokens (%.2f%%) by token\n", 100.0 * $newtypes / $types, 100.0 * $newtokens / $tokens);


sub read_file {
  my ($file) = @_;

  my %hash;

  open R1, $file or die;
  while (my $line = <R1>) {
    chomp($line);
    my ($count,@rest) = split(' ',$line);
    $hash{join(" ",@rest)} += $count;
  }
  close R1;
  
  return \%hash;
}


