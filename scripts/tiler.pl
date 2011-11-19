#!/usr/bin/env perl

# Matt Post <post@cs.jhu.edu>
# November 2011

# Takes a TSG grammar and a computes the Viterbi derivation over a
# parse tree received on STDIN.

use strict;
use warnings;
# use Heap::Simple;
use List::Util qw|reduce max min sum|;
use IO::Socket;
use Getopt::Long;
use TSG;

my $GRAMMAR_FILE = undef;
my $VOCAB_FILE   = undef;
my $VERBOSE = 0;
my $KBEST = 1;

my $optresult = GetOptions
    ("grammar=s"  => \$GRAMMAR_FILE,
     "vocab=s"    => \$VOCAB_FILE,
     "verbose=i"  => \$VERBOSE,
	 "k=i"        => \$KBEST,
    );

my (%constraints,@chart,%backpointers,%rules);

if (! defined $GRAMMAR_FILE or ! -e $GRAMMAR_FILE) {
  print "* FATAL: couldn't find grammar file (--grammar)\n";
  exit;
}

if (! defined $VOCAB_FILE or ! -e $VOCAB_FILE) {
  print "* FATAL: couldn't find vocab file (--vocab)\n";
  exit;
}

# load lexicon
my $LEXICON = read_lexicon($VOCAB_FILE, 2);

# read grammar
load_grammar($GRAMMAR_FILE);

# parse
while (my $treestr = <>) {
  chomp($treestr);

  last unless $treestr ne "";

  # read the input, and transform it to a set of constraints
  %constraints = ();
  my $tree = build_subtree($treestr);
#   my $tree = binarize_subtree({node=>build_subtree($treestr,$LEXICON),dir=>"right",collapse=>"none"});
  mark_spans($tree);
  walk($tree, [\&set_constraints]);

  my $sentence = $tree->{frontier};
  my @orig_words = split(' ', $sentence);
  my $pos = 1;
  my @words = map { lex(signature($_, $pos++)) } @orig_words;

  my $sentlen = scalar(@words);

  my $start_time = time;

  # seed the chart
  for my $pos (0..$#words) {
    add_edge({ label => $words[$pos],
          i     => $pos,
          j     => $pos + 1,
          score => 0.0 });
  }

  for (my $span = 1; $span <= $sentlen; $span++) {
    for (my $i = 0; $i <= $sentlen - $span; $i++) {
      my $j = $i + $span;

	  # print STDERR "SPAN($i..$j)\n";
#       if (exists $constraints{$i,$j}) {
#         print " [CONSTRAINTS: " . join(",", keys %{$constraints{$i,$j}}) . "]\n";
#       } else {
#         print "\n";
#       }
      # binary rules
      for (my $k = $i + 1; $k <= $j - 1; $k++) {
        while (my ($left_label,$left_edge) = each %{$chart[$i][$k]}) {
          while (my ($right_label,$right_edge) = each %{$chart[$k][$j]}) {
            my $rules = rules($left_label,$right_label);
            while (my ($lhs,$prob) = each %$rules) {
              next unless meets_constraint($lhs,$i,$j);

              my $newedge = {label => $lhs,
                             left  => $left_edge,
                             right => $right_edge,
                             i     => $i,
                             j     => $j,
                             score => $left_edge->{score} + $right_edge->{score} + log($prob)
              };

              add_edge($newedge);
            }
          }
        }
      } # end binary rules

      # unary rules
	  my @edges = values %{$chart[$i][$j]};
	  while (@edges) {
		my $edge = shift @edges;
		my $rules = rules($edge->{label});
		while (my ($lhs,$prob) = each %$rules) {
		  next unless meets_constraint($lhs, $i, $j);
		  my $newedge = {
			label => $lhs,
			i     => $i,
			j     => $j,
			left  => $edge,
			right => undef,
			score => $edge->{score} + log($prob)
		  };

		  # further explore that edge unless we've already done so
		  push(@edges, $newedge)
			  unless (exists $chart[$i][$j]{$newedge->{label}});

		  add_edge($newedge);
		}
      } 
#       if (exists $constraints{$i,$j}) {
#         foreach my $label (keys %{$constraints{$i,$j}}) {
#           if (! exists $chart{$i,$j}{$label}) {
#             print "FAILED CONSTRAINT: no $i,$j $label\n";
#           }
#         }
#       }
    }
  } # end span loop

  my $top = $chart[0][$sentlen]{TOP};

  if (! defined $top) {
    print "(())\n";
  } else {
#     my $tree = 
#       delex_subtree( # removes lexical annotations (_s)
#         parse2subtree( # produces tree rep from subtree rep, unflattens rules
#           build_subtree( # builds a subtree data structure
#             build_subtree_oneline($top,1)))); # produces string rep

    my $deriv = build_subtree_oneline(unbinarize(parse2subtree($top)),1);
    print "$deriv\n";
  }

  my $run_time = time - $start_time;
  # print STDERR "run time = $run_time seconds\n";
}


sub add_edge {
  my ($edge) = @_;
  my $label = $edge->{label};
  my $i = $edge->{i};
  my $j = $edge->{j};

  if (! exists $chart[$i][$j]{$label}) {
    $chart[$i][$j]{$label} = $edge;
  } else {
    my $old_edge = $chart[$i][$j]{$label};

    if ($edge->{score} > $old_edge->{score}) {
      $chart[$i][$j]{$label} = $edge;
    }
  }
  add_backpointer($label, $i, $j, $edge);

#   print "add_edge($edge->{label}, $i, $j)\n";
}

sub add_backpointer {
  my ($label, $i, $j, $edge) = @_;

  # don't bother unless we're going to use it
  return unless $KBEST > 1;

  if (! exists $backpointers{$label,$i,$j}) {
    # $backpointers{$label,$i,$j} = new Heap::Simple(order => sub {
    #   my ($e1, $e2) = @_;
    #   return $e1->{score} <=> $e2->{score}; });
	$backpointers{$label,$i,$j} = [];
  }

  # my $heap = $backpointers{$label,$i,$j};
  # $heap->insert($edge);

  # TODO: for kbest these need to be sorted!
  push(@{$backpointers{$label,$i,$j}}, $edge);
}

sub meets_constraint {
  my ($label, $i, $j) = @_;

  if ($label =~ /^_/ or $label =~ /^\[/ or $label =~ /^\</) {
    return 1;
  } elsif (exists $constraints{$i,$j}{$label}) {
#     print STDERR "$label [$i,$j] meets constraint\n";
    return 1;
  } else {
#     print "$label [$i,$j] FAILS constraint\n";
    return 0;
  }
}

sub set_constraints {
  my ($node) = @_;

  my $lhs = $node->{label};
  my $i   = $node->{i};
  my $j   = $node->{j};
  
#   print "CONSTRAINT: [$i," . ($j+1) . "] " . ruleof($node) . "\n";

#   print STDERR "CONSTRAINT: $lhs [$i,$j+1]\n";
  $constraints{$i,$j+1}{$lhs} = 1;
}


sub load_grammar {
  my ($file) = @_;

  if ($file =~ /\.bz2$/) {
    open GRAMMAR, "bzcat $file|" or die "can't read grammar file '$file'";
  } else {
    open GRAMMAR, $file or die "can't read grammar file '$file'";
  }

  my $start_time = time;
  while (my $line = <GRAMMAR>) {
    my ($prob, $rule) = split(' ', $line, 2);
    
    # binarize
    my $subtree = build_subtree($rule);
    my $label = $subtree->{label};
    my $frontier = $subtree->{frontier};
    my $tree = binarize_subtree({node=>$subtree,dir=>"right",collapse=>"lhs"});
    my @rules;
    extract_subtrees($tree,\@rules);
#     print "BINARIZING $line";
    foreach my $rule (@rules) {
#       print "  $rule ($prob)\n";
      $rule =~ s/^\(|\)$//g;
      my @tokens = split(' ', $rule);
      my $lhs = shift(@tokens);
      my @rhs = @tokens;
      my $rhs = join $;, @rhs;

      $rules{$rhs}{$lhs} = $prob;

#       print "\tmade rule $rule ($prob)\n";

      # rules resulting from binarization will have probability 1;
      # later, we will adjust these by pushing the mass down
      $prob = 1.0;
    }
  }
  close GRAMMAR;

  my $time_taken = time - $start_time;

  print STDERR "Grammar loading took $time_taken seconds\n";

#   foreach my $rhs (keys %rules) {
#     while (my ($lhs,$prob) = each %{$rules{$rhs}}) {
#       print "RULE($lhs -> $rhs) $prob\n";
#     }
#   }

#   my @rcounts = (0,0,0);
#   foreach my $rhs (keys %rules) {
#     my @rhs = split($;,$rhs);
#     $rcounts[scalar @rhs] += scalar keys %{$rules{$rhs}};
#   }
#   my $sum = $rcounts[2] + $rcounts[1];
#   print "Binarization produced $sum rules ($rcounts[2] binary, $rcounts[1] unary)\n" if $VERBOSE;

}

sub rules {
  my $rhs = (join $;, @_);

  my $rules = (exists $rules{$rhs}) ? $rules{$rhs} : {};
  my $numrules = scalar(keys %$rules);
#   print "FOUND $numrules RULES FOR $rhs\n";

  return $rules;
}

sub unbinarize {
  my ($node) = @_;

  map { unbinarize($_) } (@{$node->{children}});

  if ($node->{label} =~ /^\[(\S*?):/) {
    $node->{label} = "*$1";
  }

  my @kids = @{$node->{children}};
#   print "NODE: $node->{label}\n";
  for (my $i = 0; $i < @kids; ) {
    my $numkids = @kids;
    my $kid = $kids[$i];
#     print "  KID $i / $numkids ($kid->{label})\n";
    if ($kid->{label} =~ /^</) {
#       print "    REPLACING WITH TWO KIDS\n";
      splice(@kids,$i,1,@{$kid->{children}});
      next;
    }

    $i++;
  }
  $node->{children} = \@kids;

  return $node;
}


sub parse2subtree {
  my ($node) = @_;
  
  if ($node->{right}) {
    $node->{children} = [$node->{left}, $node->{right}];
  } elsif ($node->{left}) {
    $node->{children} = [$node->{left}];
  } else {
    $node->{children} = [];
  }

  $node->{numkids} = scalar @{$node->{children}};
  $node->{word} = $node->{label};

  map { parse2subtree($_) } (@{$node->{children}});

  return $node;
}


sub build_parse_oneline {
  my $node = shift;
  my $mark = shift || 0;  # whether to mark the subtrees used in the derivation
  my $score = shift || 0;

  # remove any subtree annotations
  my $label = $node->{label};
  my $subtree_mark = ($label =~ /^\[/ and $mark) ? "*" : "";
  $label =~ s/^\[(.*)\]$/$1/;

  my $str;
  # skip the printing of *binary* internal nodes
  if ($label =~ /^\<.*:.*\>$/) {
    $str = build_parse_oneline($node->{left},$mark,$score) . " " . build_parse_oneline($node->{right},$mark,$score);
  } elsif (defined $node->{left}) {
    # if this is a constituent within a subtree, it will contain the
    # information about the subtree below it -- remove this for
    # printing
#     $label =~ s/^(.+?):.*/$1/;
#     $label =~ s/^(.+?):.*/$1/;
    $str = "(${subtree_mark}$label";
    $str .= " $node->{score}" if ($score);

    if ($node->{right}) { # binary rule

      $str .= " " . build_parse_oneline($node->{left},$mark,$score);
      $str .= " " . build_parse_oneline($node->{right},$mark,$score);
    } else {
      # check whether we need to expand a collapsed unary closure
      my $lhs = $node->{label};
      my $rhs = $node->{left}->{label};
      my $UNARY_CLOSURE;
      if (exists $UNARY_CLOSURE->{$lhs}{$rhs}) {
        my @tokens = split(' ',$UNARY_CLOSURE->{$lhs}{$rhs});
        pop(@tokens);
        $str .= join(" ", map { " ($_" } @tokens);
        $str .= " " . build_parse_oneline($node->{left},$mark,$score);
        $str .= ")" x (scalar @tokens);
      } else {
        $str .= " " . build_parse_oneline($node->{left},$mark,$score);
      }
    }
    $str .= ")";
  } else {
    $str = $label;
  }

  return $str;
}
