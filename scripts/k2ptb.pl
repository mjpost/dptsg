#!/usr/bin/perl

# The Korean Treebank does not use the standard Penn Treebank format
# for trees; instead, preterminals are listed with terminals separated
# by a /.  For example, 
#
# (S (NP-SBJ *pro*) (ADJP (NP-ADV 두/NNU 개/NNX) (ADJP 있/VJ+지요/EFN)) ./SFN)
#
# To use the Korean treebank with this code, you need to convert them
# to the following representation:
#
# (S (NP-SBJ *pro*) (ADJP (NP-ADV (NNU 두)  (NNX 개) ) (ADJP (VJ 있) (EFN 지요) )) (SFN .) )
#
# This script does that.  Usage:
# 
# cat korean_treebank_files | ./k2ptb.pl
#
# The corrected trees are written to STDOUT.

use strict;
use warnings;

while (my $line = <>) {
  chomp($line);

  $line =~ s#([^ \+]+)/([^ \+\)]+)\+?#($2 $1) #g;

  print $line, $/;
}
