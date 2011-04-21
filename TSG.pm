# Matt Post <post@cs.rochester.edu>

# This file contains a long list of functions for working with trees
# in our internal representation.  This representation treats each
# node as a hash table, with children listed as an array reference in
# the value of the {children} key.
#
# The code also contains other utility functions for reading and
# writing files, etc.

package TSG;

use strict;
use Exporter;
use List::Util qw|sum max min|;
use vars qw|@ISA @EXPORT|;

@ISA = qw|Exporter|;
@EXPORT = qw| build_subtree build_subtree_oneline read_lexicon read_pos extract_subtrees extract_rules_subtree signature classof mark_spans mark_subtree_height count_subtree_lex count_subtree_frontier count_subtree_nodes prune pruneit lex delex islex delex_tree walk walk_postorder frontier lhsof $LEXICON $LEXICON_THRESH ruleof is_terminal is_preterminal process_params scrub_node mark_parent mark_heads binarize_grammar binarize_subtree push_weights|;

require "$ENV{DPTSG}/head-rules-chiang.pl";

## VARIABLES
# a list of invalid parts of speech, used by the prune() function
my @INVALID_POS = qw[-NONE-]; # '' `` -RRB- -LRB-];

# the lexicon and lexicon threshold, set in read_lexicon()
my $LEXICON;
my $LEXICON_THRESH;

# lex(): puts a word into lexical representation (surrounded by _s).
# See README.code for reasons why we need to do this (basically, since
# our subtree representation may be a subtree fragment, we need a way
# to distinguish terminals from nonterminals, both of which may be on
# the leaves; and we can't assume that the sets are disjoint, because
# they're not).
#
# lex("word") => "_word_"
sub lex {
  my $arg = shift;
  return $arg if islex($arg);
  return "_" . $arg . "_";
}

# removes lexical annotations
# delex("_word_") -> "word"
sub delex {
  my $arg = shift;
  # we don't want to remove underscores internal to a word
  $arg =~ s/^_|_$//g if islex($arg);
  return $arg;
}

# returns true if the word is in lexical representation, false
# otherwise
sub islex {
  my $arg = shift;
  my $islex = ($arg =~ /^_.+_$/) ? 1 : 0;
  return $islex;
}

# used by prune -- returns true if node should be pruned
sub pruneit {
  my $node = shift;
  my @kids = @{$node->{children}};
  my $prune;

  # base case -- prune preterminals with an invalid POS
  if (is_preterminal($node)) {
    my $label = $node->{label};
    $prune = grep /^$label$/, @INVALID_POS;
  } elsif (@{$node->{children}}) {
    # recursive case -- prune interior nodes whose all children are removed
    my $count = sum map { pruneit($_) } @kids;
    $prune = ($count == scalar @kids) ? 1 : 0;
  }

  return $prune;
}

# takes a subtree and prunes bad nodes from it; used to remove trace
# elements from trees as a preprocessing step
sub prune {
  my $node = shift;

  map { prune($_) } @{$node->{children}};

  my (@keep,@prune);
  foreach my $kid (@{$node->{children}}) {
    if (pruneit($kid)) {
      push(@prune,$kid);
    } else {
      push(@keep,$kid);
    }
  }

  # removing children can result in a node of the form X -> X; if you
  # see this, delete the child node by making its children our children
  while (1 == @keep and $node->{label} eq $keep[0]->{label}) {
    @keep = @{$keep[0]->{children}};
  }

  if (@keep) {
    $node->{children} = \@keep;
    $node->{numkids} = scalar @keep;
    my $hpos = &head_pos(clean($node->{label}), map {clean($_->{label})} @keep);
    $node->{hpos} = $hpos;
    $node->{head} = $keep[$hpos]->{head};
    $node->{headtag} = $keep[$hpos]->{headtag};
    $node->{rule} = $node->{label}." -> ".join(' ', map {$_->{label}} @keep);
#     print "NEW NODE: $node->{label} $node->{hpos} $node->{head} $node->{headtag} $node->{rule}\n";
  }

  $node;
}

# builds the rule representation of a node (of arbitrary depth, if it's
# the root of a large subtree).  Include a second argument if you want
# just the depth-one rule, even if it's the root of a subtree.
sub ruleof {
  my ($node,$stop) = @_;

  my $label = $node->{label};
  $label =~ s/^\*//;

  my $str;
  if (scalar @{$node->{children}}) {
    $str = "($label";
    foreach my $kid (@{$node->{children}}) {
      if (!$stop and $kid->{label} =~ /^\*/) {
        $str .= " " . ruleof($kid);
      } else {
        my $lab = $kid->{label};
        $lab =~ s/^\*//;
        $str .= " " . $lab;
      }
    }
    $str .= ")";
  } else {
    $str .= $node->{label};
  }
  return $str;
}

# reads in the lexicon into a hash table
# the lexicon has lines of the form
# id lexeme count
sub read_lexicon {
  my ($lex_file,$thresh) = @_;
  $LEXICON_THRESH = (defined $thresh) ? $thresh : 2;
  $LEXICON = {};

  open LEX, $lex_file or die "can't open vocabulary file '$lex_file'";
  while (my $line = <LEX>) {
    chomp $line;
    next if /^#/;
    my ($id,$word,$count) = split ' ', $line;
    $LEXICON->{$word} = $count;
  }
  return $LEXICON;
}


# read parts of speech for each word (not often used)
sub read_pos {
  my $pos_file = shift;
  my $POS = {};

  open POS, $pos_file or die "can't read lexicon '$pos_file'";
  while (my $line = <POS>) {
    chomp $line;
    next if /^#/;
    my ($word,$pos) = split(' ',$line);
    $POS->{$word}{$pos} = 1;
  }

  return $POS;
}

# determines the signature of a word.  A word's signature is either
# the word itself or one of the UNK categories, based on the frequency
# of the word and command-line options.
sub signature {
  my ($word,$pos) = @_;
  $pos = -1 unless defined $pos;

  my $argword = $word;
  $word = delex($word);

  my $sig = "UNK";

  if (! defined $LEXICON) {
    # no lexicon, can't transform
    $sig = $argword;
  } elsif ($word =~ /^UNK/) {
    # word is already a signature
    $sig = $argword;
  } elsif ( ($LEXICON_THRESH == 0) or (exists $LEXICON->{$word} and $LEXICON->{$word} >= $LEXICON_THRESH)) {
    # above threshold, no need to transform
    $sig = $argword;
  } else {
    $sig = classof($word,$pos);
  }

#  print "SIGNATURE($argword) = $sig\n" unless $sig eq $argword;

  return $sig;
}

# turns a word into its UNK class.  Called by signature()
sub classof {
  my ($word,$pos) = @_;
  $word = delex($word);
  $pos = -1 unless defined $pos;
  my $lowered = lc($word);

  my $len = length($word);

  my $sig = "UNK";

  my $hasDigit = ($word =~ /[0-9]/) ? 1 : 0;
  my $hasLower = ($word =~ /[a-z]/) ? 1 : 0;
  my $hasDash  = ($word =~ /\-/)    ? 1 : 0;
  my $numCaps = 0;
  $numCaps++ while $word =~ /[A-Z]/g;

  if ($word =~ /^[A-Z]/) {
    if ($pos == 1 && $numCaps == 1) {
      $sig .= "-INITC";
      if (exists $LEXICON->{$lowered}) {
        $sig .= "-KNOWNLC";
      }
    } else {
      $sig .= "-CAPS";
    }
  } elsif ($word =~ /^[^a-zA-Z]/ and $numCaps > 0) {
    $sig .= "-CAPS";
  } elsif ($hasLower) {
    $sig .= "-LC";
  }

  if ($hasDigit) {
    $sig .= "-NUM";
  }
  # 2010-08-18 removed this since it is rare and mostly causes problems
  # if ($hasDash) {
  #   $sig .= "-DASH";
  # }

  if ($len >= 3 && $lowered =~ /s$/){
    $sig .= "-s" if !($lowered =~ /ss$/ || $lowered =~ /us$/ || $lowered =~ /is$/);
  } elsif ($len >= 5 && !$hasDash && !($hasDigit and $numCaps > 0)) {
    $sig .= "-ed", if ($lowered =~ /ed$/);
    $sig .= "-ing", if ($lowered =~ /ing$/);
    $sig .= "-ion", if ($lowered =~ /ion$/);
    $sig .= "-er", if ($lowered =~ /er$/);
    $sig .= "-est", if ($lowered =~ /est$/);
    if ($lowered =~ /ly$/) {
      $sig .= "-ly";
    } elsif ($lowered =~ /ity$/) {
      $sig .= "-ity";
    } elsif ($lowered =~ /y$/) {
      $sig .= "-y";
    }
    $sig .= "-al" if ($lowered =~ /al$/);
  }
  return $sig;
}

# builds a tree structure from a one-line textual representation
sub build_subtree {
  my ($line,$lexicon) = @_;

  # minimal sanity check
  if ($line !~ /^\(.*\)\s*$/) {
    return undef;
  }

  $line =~ s/\(/ \(/g;
  $line =~ s/\)/ \) /g;
  my @a = split ' ', $line;
  my $c;
  my @c;
  my $top;
  my @words;  # the words of the sentence
  while (@a) {
    my $token = shift @a;
    if ($token =~ s/^\(//) { ### new node
      $c = {};
#       $c->{parent} = $c[-1]->{label} unless $#c < 0;
      $c->{label} = $token;
      $c->{children} = [];

      # add oneself to one's parent's list of children
      push(@{$c[-1]->{children}}, $c) unless $#c < 0;

      # add oneself to the stack of parents
      push(@c, $c);
    } elsif ($token =~ /^\)/) { ### end of node
      # remove one item from the stack of parents
      $top = pop(@c);

      # check for common error
      if (! defined $top->{children}) {
        print STDERR "* [$.] WARNING: $c->{label} has no children (token $token)\n";
        return undef;
      }

      # set the number of children I have (since we know it now)
      $top->{numkids} = scalar @{$top->{children}};
      $top->{frontier} = join(' ', map { $_->{frontier} } @{$top->{children}});
      # compute the head position
      my $rhs = join(" ", map {$_->{label}} @{$top->{children}});
      # my $hpos = &head_pos(clean($top->{label}), map {clean($_->{label})} @{$top->{children}});
      # print "* WARNING: no head pos for rule '$top->{label} -> $rhs'\n" if (-1 == $hpos);
#       print "HPOS($top->{label} -> ", (join " ", map {$_->{label}} @{$top->{children}}), ") = $hpos\n";
      # $top->{hpos} = $hpos;
      # $top->{head} = @{$top->{children}}[$hpos]->{head};
      # $top->{headtag} = @{$top->{children}}[$hpos]->{headtag};

      #compute the depth
      $top->{depth} = 1 + max(map { $_->{depth} } @{$top->{children}});

    } else { ### leaf (also new node)

      push(@words,$token);

      $c = {};
      $c->{label} = $lexicon ? lex(signature($token,scalar @words)) : $token;
      $c->{word} = $token;
      $c->{children} = [];
      $c->{numkids} = 0;
      $c->{frontier} = $token;
      $c->{depth} = 0;
      # $c->{head} = $lexicon ? lex(signature($token)) : $token;

      # add oneself to one's parent's list of children

      push(@{$c[-1]->{children}}, $c); # unless $#c < 0;
      # set the parent's depth
      $c[-1]->{depth} = 1;
      # $c[-1]->{headtag} = $c[-1]->{label};

#      push @c, $c;
#      $c = pop @c;
#      shift @a; # dispose of following close paren
    }
  }
#    print &print_parse_paren($top). "\n";
#   @{$top->{children}}[0];
  $top;
}

# removes treebank 2 information from nodes
sub scrub_node {
  my ($node) = @_;

  if (@{$node->{children}}) {
    $node->{label} =~ s/(.)-.+[-.+]?/$1/;  # remove treebank2 info
    $node->{label} =~ s/=.+//;
  }
}

# takes a node and converts the subtree beneath it to a one line
# textual representation. If a second argument is passed, the leaves
# are stripped of the lexical markers (_s).
sub build_subtree_oneline {
  my ($node,$delex) = @_;

  my $str;
  my $numkids = scalar @{$node->{children}};
  if ($numkids) {
    $str = "($node->{label}";
    # annotate heads
#     @{$node->{children}}[$node->{hpos}]->{label} =~ s/^/\+/;
    map { $str .= " " . build_subtree_oneline($_,$delex) } @{$node->{children}};
    $str .= ")";
  } else {
    $str = $delex ? delex($node->{word}) : $node->{label};
  }

  return $str;
}

sub escape {
  my $arg = shift;
  $arg =~ s/\\/\\\\/g;  # do this first since more \s will be added
  $arg =~ s/\./\\\./g;
  $arg =~ s/\?/\\\?/g;
  $arg =~ s/\$/\\\$/g;
  $arg =~ s/\'/\\\'/g;
  $arg =~ s/\!/\\\!/g;
  $arg =~ s/\-/\\\-/g;
  $arg =~ s/\#/\\\#/g;
  $arg =~ s/\%/\\\%/g;
  $arg =~ s/\|/\\\|/g;
  return $arg;
}

# is_internal
#
# returns true if the node is internal to a subtree, false otherwise
sub is_internal {
  my ($node) = @_;

  return ($node->{label} =~ /^\*/) ? 1 : 0;
}

# extract_subtrees
#
# takes a tree or subtree and extracts all the subtrees found in it,
# adding them to the list passed to it
sub extract_subtrees {
  my ($node,$list) = @_;

  # base case -- child
  return unless $node->{numkids};

  # root of subtree
  if (! is_internal($node)) {
    push(@$list,ruleof($node));
  }

  # recursive call
  map { extract_subtrees($_,$list) } @{$node->{children}};
}


# extract_rules_subtree [deprecated]
#
# takes a node and a list reference, and recursively finds each
# (depth-one) rule, adding it to the list.
sub extract_rules_subtree {
  my $node = shift;
  my $rules = shift;

  # base case -- child
  return unless $node->{numkids};

  # build the rule
  # record the rule representation
  my $rule = "($node->{label} " . (join " ", map {$_->{label}} @{$node->{children}}) . ")";
  push @$rules, $rule;
#   print "EXTRACT: $rule\n";
  
  # recurse
  map { extract_rules_subtree($_,$rules) } @{$node->{children}};
}

# binarize_subtree
#
# takes arguments as a hash: 
# - node: the node that is the subtree root
# - unique: annotate binarized nodes to uniquely identify the subtree rooted at them
# - dir (left,right): type of binarization
# - collapse (lhs,@,none): 
# -- lhs: include LHS in binarization
# -- @: prepend @ sign to binarized nodes (a la Berkeley binarization)
# -- none: create new nonterminal name by concatening the children
sub binarize_subtree {
  my $args = shift;
  my %defaults = (
    node => undef,  # node to binarize
    not_root => 0,  # true if this is not the root of a tree/subtree
    unique => 1,    # annotate nodes to be part of a subtree
    dir => "right", # left or right binarization
    collapse => "none",  # lhs:put lhs in binarized name; @: prefix @ sign
  );
  map { $args->{$_} = $defaults{$_} unless defined $args->{$_} } keys %defaults;

  my $node     = $args->{node};
  my $unique   = $args->{unique};
  my $not_root = $args->{not_root};
  my $dir      = $args->{dir};
  my $collapse = $args->{collapse};

  # base case: nothing more to do
  return unless $node->{numkids};

  map { binarize_subtree({node=>$_,dir=>$dir,not_root=>1,unique=>$unique,collapse=>$collapse}) } @{$node->{children}};

  # binarize to a right-branching structure
  while ($node->{numkids} > 2) {
    my $newnode = {};
    if ($dir eq "right") {
      $newnode->{children} = [splice @{$node->{children}},$node->{numkids}-2,2,$newnode];
    } elsif ($dir eq "left") {
      $newnode->{children} = [splice @{$node->{children}},0,2,$newnode];
    } elsif ($dir eq "terminal") {
	  # binarize terminals first, otherwise do right binarization
	  my $i;
	  for ($i = 0; $i + 1 < @{$node->{children}}; $i++) {
		my $kid = @{$node->{children}}[$i + 1];
		last if ($kid->{label} =~ /^_.*_$/);
	  }
	  $i-- if ($i == (@{$node->{children}} - 1));
	  $newnode->{children} = [splice @{$node->{children}},$i,2,$newnode];
	}
    my $kidlabels = join ":", map { $_->{label} } @{$newnode->{children}};
#     $newnode->{label} = "<$node->{label}:$kidlabels>";  # :$id
#     $newnode->{label} = "<$kidlabels>";  # :$id

    if ($collapse eq "lhs") {
      $newnode->{label} = "<$node->{label}:$kidlabels>";
    } elsif ($collapse eq "@") {
      $newnode->{label} = "\@$node->{label}";
    } else {
      $newnode->{label} = "<$kidlabels>";
    }

#     $newnode->{label} = "[$newnode->{label}]" if ($unique);
    $newnode->{numkids} = 2;
    $node->{numkids}--;
  }

  # now handle the remaining (or original, if no binarization happened)
  # binary rule, and handle unary rules. *all* internal nodes need to
  # be marked in a way that unique identifies the subtree rooted at them.
#   $node->{label} = "[$node->{label}:" . (join ':',map {$_->{label}} @{$node->{children}}) . "]" if ($not_root and $unique);
  $node->{label} = "[" . (join ':',map {$_->{label}} @{$node->{children}}) . "]" if ($not_root and $unique);

  return $node;
}

# mark_heads
#
# annotate each node with its head child
sub mark_heads {
  my ($node) = @_;

  if (is_preterminal($node)) {
    my $kid = @{$node->{children}}[0];
    $node->{hpos} = 0;
    $node->{head} = $kid->{label};
    $node->{headtag} = $node->{label};

  } elsif (@{$node->{children}}) {
    my $rule = ruleof($node,1);
    $rule =~ s/[\(\)]//g;
    my $hpos = &head_pos(split(' ',$rule));
    print "* WARNING: no head pos for rule '$rule'\n" if (-1 == $hpos);
    # print "HPOS($rule) = $hpos\n";

    $node->{hpos} = $hpos;
    $node->{head} = @{$node->{children}}[$hpos]->{head};
    $node->{headtag} = @{$node->{children}}[$hpos]->{headtag};
  }
}

# mark_subtree_height
#
# annotate ach node with the maximum distance to any of the leaves in
# its frontier
sub mark_subtree_height {
  my $node = shift;

  if (! @{$node->{children}}) {
    $node->{height} = 0;
    return $node;
  }    

  map { mark_subtree_height($_) } @{$node->{children}};

  $node->{height} = 1 + max( map { $_->{height} } @{$node->{children}});
  return $node;
}

# count_nodes
#
# counts the number of nodes in a subtree
sub count_subtree_nodes {
  my $subtree = shift;

  return 1 + sum(map {count_subtree_nodes($_)} @{$subtree->{children}});
}

# count_subtree_lex
#
# counts the number of lexical items among the leaves of a subtree
sub count_subtree_lex {
  my $subtree = shift;

  if (scalar @{$subtree->{children}}) {
    return sum(map { count_subtree_lex($_) } @{$subtree->{children}});
  } else {
    return islex($subtree->{label});
  }
}

# extracts the frontier elements from a string representation
sub extract_frontier {
  my $rep = shift;
  $rep =~ s/\)//g;
  return join(" ", grep(!/^\(/, split(' ',$rep)));
}

# frontier
# arg1: the node from which to search for the frontier
sub frontier {
  my ($node) = @_;

  (@{$node->{children}}) 
      ? join(" ", map { frontier($_) } @{$node->{children}})
      : delex($node->{label});
}

# counts leaves in a subtree, both nonterminal and terminal
sub count_subtree_frontier {
  my $subtree = shift;
  my $numkids = scalar @{$subtree->{children}};
  return 1 if (0 == $numkids);
  return sum(map { count_subtree_frontier($_) } @{$subtree->{children}});
}

# walk_postorder, walk_preorder
# 
# Generic function for walking the nodes of a tree in depth-first
# order.  At each node, all of the functions in {funcs} (an array
# reference) are applied to the node, in order.  Each of these
# functions receives the current node as well as whatever {rest} is.
# 
# walk_postorder: functions are called *after* the recursive call.
# walk_preorder: functions are called *before* the recursive call.
# walk: defaults to walk_preorder
sub walk_postorder {
  my ($node,$funcs,$rest) = @_;
  # recurse
  map { walk_postorder($_,$funcs,$rest) } @{$node->{children}};
  # apply functions
  map { $_->($node,$rest) } @$funcs;
  return $node;
}
sub walk_preorder {
  my ($node,$funcs,$rest,@results) = @_;
  # print "WALK($node->{label})\n";
  my @newresults = map { $_->($node,$rest,@results) } @$funcs;
  # print "RESULTS:\n";
  # map { print "  $_\n" } @results;
  map { walk_preorder($_,$funcs,$rest,@newresults) } @{$node->{children}};
  return $node;
}
sub walk {
  return walk_preorder(@_);
}

# delex_node
#
# removes the lexical markers from the node label
sub delex_node {
  my $node = shift;
  $node->{label} = delex($node->{label}) if $node->{label};
}

# delex_subtree
# 
# removes lexical markers from all nodes in a subtree
sub delex_subtree {
  my $node = shift;
  return walk($node,[\&delex_node]);
}

# returns the lhs symbol of a rule from its string representation:
# e.g., (S (NP (DT the) (NN boy) (VP (VBD was) VP))) => S
sub lhsof {
  my $arg = shift;
  my $lhs = (split ' ', $arg)[0];
  $lhs =~ s/^\(//;
  return $lhs;
}

# returns true if the node is a terminal
sub is_terminal {
  my ($node) = @_;
  if (! exists $node->{children} || (0 == scalar @{$node->{children}})) {
    return 1;
  }
  return 0;
}

# returns true if the node is a preterminal
sub is_preterminal {
  my ($node) = @_;

  # if (! defined $node->{children}) {
  #   print "NO KIDS FOR $node->{label}\n";
  #   exit;
  # }

  if (1 == @{$node->{children}}) {
    my $kid = @{$node->{children}}[0];
    if (0 == @{$kid->{children}}) {
      return 1;
    }
  }
  return 0;
}

# process_params
#
# Takes the list of default parameters, the arguments list from the
# command line, and the set of environment variables, and sets
# parameters based on them.  Command-line arguments are given highest
# priority; next, environment variables; finally, defaults.
sub process_params {
  my ($PARAMS,$ARGV,$ENV) = @_;
  my %binary;   # params (starred) that don't take args, default to 0
  foreach my $key (keys %$PARAMS) {
    if ($key =~ /^\*/) {
      $key =~ s/^\*//;
      $binary{$key} = 1;
    }
    if (exists $ENV->{$key}) {
      $PARAMS->{$key} = $ENV->{$key};
      print STDERR "* $key = $PARAMS->{$key} [env]\n";
    }
  }
  # delete starred version of arguments (which are binary ones)
  map { $PARAMS->{$_} = $PARAMS->{"*$_"};
        delete $PARAMS->{"*$_"}; } keys %binary;

  # process command-line arguments
  while (@$ARGV) {
    my $arg = shift @$ARGV;

    die "invalid option '$arg'" unless $arg =~ /^-/;

    $arg =~ s/^-//g;

    if (exists $PARAMS->{$arg}) {
      # binary arguments are true when present, only optionally take
      # an argument (for backward compatibility)
      if ($binary{$arg} and (! @$ARGV or $ARGV->[0] =~ /^-/)) {
        $PARAMS->{$arg} = 1;
      } else {
        $PARAMS->{$arg} = shift @$ARGV;
      }
      print STDERR "* $arg = $PARAMS->{$arg} [cmdline]\n";
    } else {
      die "no such option '$arg'";
    }
  }
}

# clean
# 
# removes marker that indicates nodes internal to a subtree
sub clean {
  my ($label) = @_;
  $label =~ s/^\*//;

  return $label;
}

# mark_parent
#
# adds a field to each node pointing to its parent node
sub mark_parent {
  my ($node) = @_;
  map { $_->{parent} = $node } @{$node->{children}};
}

# returns the level of binarization (the distance from the bottom of
# the binarization) of a nonterminal, e.g.,
# S                  => 0
# <S:NP:VP>          => 1
# <S:NP:<VP:VBD:VP>> => 2
sub bin_level {
  my $arg = shift;
  my $lhs = (split(' ',$arg))[0];
  my $count = 0;
  $count++ while $lhs =~ /</g;
  return $count;
}

sub mark_spans {
  my ($node,$index) = @_;
  $index = 0 unless defined $index;

  if (! @{$node->{children}}) {
    $node->{i} = $index;
    $node->{j} = $index;
    return $index + 1;
  } else {
    my $old = $index;
    map {
      $index = mark_spans($_,$index)
    } @{$node->{children}};
    $node->{i} = $old;
    $node->{j} = $index - 1;
    return $index;
  }
}

# binarizes a grammar using the greedy substring-matching binarization
# approach
sub binarize_grammar {
  my ($rulesarg) = @_;

  my (%rules,%pmap,%notdone,%counts,%rulemap);

  # 1. count all frontier pairs, and map them to the rule they appear in
  while (my ($rule,$prob) = each %$rulesarg) {
#     print "RULE($rule) $prob\n";
    my ($lhs,@leaves) = split(' ',$rule);
    my $leaves = join(" ",@leaves);
    if (@leaves > 2) {
      $notdone{$lhs}{$leaves} = $prob;
      map { $counts{$lhs}{$leaves[$_-1],$leaves[$_]}++ } (1..$#leaves);
    } else {
      $rules{join($;,@leaves)}{$lhs} = $prob;
    }
    $rulemap{"$lhs $leaves"} = "$lhs $leaves";
  }

  # 2. greedily reduce pairs until no more remain
  foreach my $lhs (keys %counts) {

    while (scalar keys %{$notdone{$lhs}}) {
      # find the max pair in each rule, binarize that
      my %postponed;
      foreach my $leaves (keys %{$notdone{$lhs}}) {
        my @leaves = split(' ',$leaves);
        my $bestpair = undef;
        my $bestcount = 0;
        my $bestpos = -1;
        for my $i (1..$#leaves) {
          my $pair = "$leaves[$i-1] $leaves[$i]";
          my ($l,$r) = ($leaves[$i-1],$leaves[$i]);
#           my $label = "<$lhs:$l:$r>";
#           my $label = "<$l:$r>";
          my $label = "$l:$r";
          # only allow a particular binarization to occur once per rule
          if ($counts{$lhs}{$l,$r} > $bestcount) { # && ! exists $pmap{"$lhs $leaves"}{"$label $l $r"}) {
            $bestcount = $counts{$lhs}{$l,$r};
            $bestpair = $pair;
            $bestpos = $i;
          }
        }

#         print "RULE($lhs $leaves)\n";
#         print "  BEST($bestpos,$bestpair,$bestcount)\n";

        # subtract all the counts
        map { $postponed{$leaves[$_-1],$leaves[$_]}-- } (1..$#leaves);
#          map { $counts{$lhs}{$leaves[$_-1],$leaves[$_]}-- } (1..$#leaves);

        # make the replacement
        my ($l,$r) = split(' ',$bestpair);
#         my $label = "<$lhs:$l:$r>";
#         my $label = "<$l:$r>";
        my $label = "$l:$r";

        # create new rule, and adjust the list of binarized rules used
        # by the top-level parent (which now has a new name)
        $rules{$l,$r}{$label} = 1.0;  # record the rule
        splice(@leaves,$bestpos-1,2,($label)); # insert binarized rule
        my $newleaves = join(" ",@leaves); # new leaves string
        $pmap{"$lhs $newleaves"} = $pmap{"$lhs $leaves"}; # rename parent
        delete $pmap{"$lhs $leaves"}; # delete old parent
        $pmap{"$lhs $newleaves"}{"$label $l $r"}++; # count new child
        
        # update the map between the original rule and its top-level
        # binarized piece
        if ($newleaves ne $leaves) {
          $rulemap{"$lhs $newleaves"} = $rulemap{"$lhs $leaves"};
          delete $rulemap{"$lhs $leaves"};
        }

        # increment the counts
        map { $postponed{$leaves[$_-1],$leaves[$_]}++ } (1..$#leaves);
#         map { $counts{$lhs}{$leaves[$_-1],$leaves[$_]}++ } (1..$#leaves);

        # update
        my $prob = $notdone{$lhs}{$leaves};
        delete $notdone{$lhs}{$leaves};
        if (@leaves > 2) {
          $notdone{$lhs}{join(" ",@leaves)} = $prob;
        } elsif (@leaves == 2) {
#           print "TOP($lhs -> $newleaves) = $prob\n";
          $rules{join($;,@leaves)}{$lhs} = $prob;
        }
      }

        # update counts if we're not done
      if (scalar keys %{$notdone{$lhs}}) {
        map { $counts{$lhs}{$_} += $postponed{$_} } keys %postponed;
      }
    }
  }

  # debugging
#   while (my ($parent,$hash) = each %pmap) {
#     print "PARENT RULE: $rulemap{$parent}\n";
#     my ($lhs,@rhs) = split(' ',$parent);
#     my $rhs = join($;,@rhs);
#     my $prob = $rules{$rhs}{$lhs};
#     print "  $parent ($prob)\n";
#     while (my ($key,$prob) = each %{$pmap{$parent}}) {
#       print "  $key ($prob)\n";
#     }
#   }

  # convert the pmap (where a parent rule lists all of the binarized
  # pieces it was turned into) into the binmap (in which each binary
  # segment points to all of the parent rules it is part of)
  my %pieces_to_parents;
  foreach my $rule (keys %pmap) {
    # each binary rule points to its parent, and its value is the
    # number of times it appears beneath that parent
    map { $pieces_to_parents{$_}{$rule} = $pmap{$rule}{$_} } keys %{$pmap{$rule}};
#     map { $binmap{$_}{$rule} = 1.0 } keys %{$pmap{$rule}};
  }

  # rules: the binarized rules { rhs => { lhs => prob } }
  # pieces_to_parents: maps binarized rule pieces to original parent
  # rulemap: maps top-level pieces to original rule strings
  return (\%rules,\%pieces_to_parents,\%rulemap);
}

1;
