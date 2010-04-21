#!/usr/bin/perl

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;

# load the map

my %rule_mapping;

my $map_file = shift @ARGV;
open READ, $map_file or die "can't open file $map_file";
while (my $line = <READ>) {
  chomp($line);
  my ($flat,$full) = split(/ \|\|\| /,$line);
  
  $rule_mapping{$flat} = $full;
}
close READ;

# subtree-internal nodes MUST be annotated to prevent an infinite
# amount of recursion; for example, consider the rule 
#   NP-4 -> @NP-2 DT-0
# mapping to
#   NP-4 -> (NP-4 (*NP-4 @NP-2 DT-0))
# if the internal NP-4 is not marked, then the recursive call will
# result in infinite expansion
my $annotate = 1;

my (%rules,%map);

while (my $line = <>) {
  chomp($line);
  if ($line eq "(TOP)") {
    print "(TOP)\n";
    next;
  }

  my $tree = build_subtree($line);

  walk($tree,[\&unflatten]);

  walk($tree,[\&remove_mark]);

  print build_subtree_oneline($tree,1), $/;
}

sub substitute_subtree {
  # node is the current node in the subtree, and nodes is the list of
  # nodes yest to be matched with the leaves of the subtree node is a
  # part of
  my ($node,$nodes,$isroot) = @_;
  $isroot = 1 unless defined $isroot;

  if ($annotate) {
    $node->{label} = "*" . $node->{label} unless $isroot;
  }

  my $numkids = @{$node->{children}};

#   print "  SUB($node->{label},[", join(" ",map{$_->{label}}@$nodes), "]) $numkids kids\n";

  for my $k (0..$#{$node->{children}}) {
    my $kid = @{$node->{children}}[$k];
    if ($kid->{numkids} == 0) {
      my $repl = shift @$nodes;
#         print "   SUBBING $kid->{label} = $repl->{label}\n";
      @{$node->{children}}[$k] = $repl;
    } else {
      substitute_subtree($kid,$nodes,0);
    }
  }

  return $node;
}

sub unflatten {
  my ($node) = @_;

  if ($node->{children}) {

    my $rule = "$node->{label} --> " . join(" ",map {$_->{label}} @{$node->{children}});

    if (exists $rule_mapping{$rule}) {
      my $full_rule = $rule_mapping{$rule};

      my $subtree = build_subtree($full_rule);
      my @kids = @{$node->{children}};
      substitute_subtree($subtree,\@kids);
      $node->{children} = $subtree->{children};
    }
  }
}

# removes markings denoting internal nodes
sub remove_mark {
  my ($node) = @_;

  $node->{label} =~ s/^\*//;
}
