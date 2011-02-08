#!/usr/bin/perl

# samples parse trees from a grammar
# usage:
# sample.pl -grammar <grammar file> -n <num samples> -len <max sentlen>

my $basedir;
BEGIN {
  $basedir = $ENV{DPTSG};
  unshift @INC, $basedir;
}

use strict;
use warnings;
use TSG;
# use tsg_utils;

my %PARAMS = (
  n => 1,               # number of samples
  grammar => undef,     # grammar to use
  len => 1000,          # maximum length of each sample
  v => 0,               # verbose
  lexicon => "$basedir/data/lex.02-21",
  thresh => 1
);
process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

my ($grammar,$cdf) = load_grammar($PARAMS{grammar});

# TESTING
# my %counts;
# my $total = 0;
# for (;;) {
#   my $i = rand_rule($cdf->{TOP});
#   $total++;
#   $counts{$i}++;
#   print "$i $counts{$i} ", (100.0 * $counts{$i} / $total), "%\n";
# }

for (my $i = 0; $i < $PARAMS{n}; ) {
  my $top = build_subtree($grammar->{TOP}->[rand_rule($cdf->{TOP},"TOP")]);

  # sampling from some grammars results in near-infinite recursion, so
  # we count the number of expansions we do and quit if it gets to be
  # too many
  my $expanded = $PARAMS{len} ** 2;

  my $sample = sub {
    my ($node) = @_;
    my $lhs = $node->{label};

    # find nonterminals on the frontier, and replace them with sampled
    # subtrees
    if (! @{$node->{children}}) {
      if (! islex($node->{label})) {
        my $new_subtree = build_subtree($grammar->{$lhs}->[rand_rule($cdf->{$lhs},$lhs)]);

        my $subtree = build_subtree_oneline($new_subtree);
        # print STDERR "[$i] expanding $lhs to $subtree\n";
        $node->{children} = $new_subtree->{children};
        $node->{numkids} = scalar @{$node->{children}};
        goto quit if (--$expanded < 0);
      }
    }
  };

  walk($top,[$sample]);

  my @sent = split(' ',leaves($top));
  my $sentlen = scalar @sent;
  if ($sentlen <= $PARAMS{len}) {
    # print join(" ",@sent), $/;
    print build_subtree_oneline($top,1), $/;
    $i++;
  }
  next;

 quit: {
   print STDERR "breaking out of unbounded recursion\n";
   next;
  }

}

# print $top->{frontier}, $/;
# print "$idx $rule\n";
# my $top = build_subtree(@{$grammar->{TOP}}[rand_rule($cdf->{TOP})]);
# print $top->{frontier}, $/;

sub load_grammar {
  my ($file) = @_;

  $| = 1 if $PARAMS{v};

  my (%grammar,%cdf);
  open FILE, $file or die "can't load grammar from '$file'";
  my $lineno = 0;
  while (my $line = <FILE>) {
    chomp($line);
    next if $line =~ /^\s*$/;

    $lineno++;

    print STDERR "\rread $lineno..." if $PARAMS{v} and ! ($lineno % 10000);

    my ($prob,@rest) = split(' ',$line);
    my $rule = join(" ",@rest);

    my $lhs = $rest[0];
    $lhs =~ s/\(//;

    push(@{$grammar{$lhs}},$rule);
    my $current_mass = $cdf{$lhs} ? @{$cdf{$lhs}}[-1] : 0;
    push(@{$cdf{$lhs}},$current_mass + $prob);
  }
  close FILE;

  return (\%grammar,\%cdf);
}

# do a binary search to find the appropriate rule
# array is a CDF, not a PDF!
sub rand_rule {
  my ($cdf,$lhs) = @_;

  my $prob = rand;

  if (! $cdf) {
    print "BAD LHS $lhs\n";
    exit;
  }

  my $i = 0;
  my $j = (scalar @{$cdf}) - 1;

  # print "RAND_RULE\n";
  do {
    my $mid = int(($j-$i)/2) + $i;
    my $cur = $cdf->[$mid];
    # print "  $i $j $prob cdf[$mid] = $cur\n";
    if ($cur < $prob) {
      $i = $mid + 1;
    } elsif ($cur >= $prob) {
      $j = $mid;
    }
  } while ($i != $j);

  # print "DONE ($i).\n";

  return $i;
}
    
sub leaves {
  my ($node) = @_;

  if (scalar @{$node->{children}}) {
    return join(" ", map { leaves($_) } @{$node->{children}});
  } else {
    return delex($node->{label}), " ";
  }
}
