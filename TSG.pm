package TSG;

use strict;
use Exporter;
use vars qw|@ISA @EXPORT|;

@ISA = qw|Exporter|;
@EXPORT = qw| build_subtree build_subtree_oneline read_lexicon read_pos extract_rules_subtree signature classof mark_subtree_height count_subtree_lex count_subtree_frontier prune pruneit lex delex islex delex_tree walk walk_postorder frontier lhsof $LEXICON $LEXICON_THRESH ruleof is_terminal is_preterminal process_params scrub_node mark_parent mark_heads binarize_grammar push_weights|;

require "$ENV{HOME}/code/dpinfer/head-rules-chiang.pl";

use Memoize;
use List::Util qw|sum max min|;

my @INVALID_POS = qw[-NONE-]; # '' `` -RRB- -LRB-];

my $LEXICON;
my $LEXICON_THRESH;

sub lex {
  my $arg = shift;
  return $arg if islex($arg);
  return "_" . $arg . "_";
}

sub delex {
  my $arg = shift;
#   $arg =~ s/^_|_$//g;
  $arg =~ s/_//g;
  return $arg;
}

sub islex {
  my $arg = shift;
  my $islex = ($arg =~ /^_.*_$/) ? 1 : 0;
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

# takes a subtree and prunes bad nodes from it
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

# builds the rule (of arbitrary depth), but you can get the depth-one
# rule with a second arg
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

sub signature {
  my ($word) = @_;

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
    $sig = classof($word);
  }

  return $sig;
}

sub classof {
  my ($word) = @_;
  $word = delex($word);
  my $lowered = lc($word);

  my $len = length($word);

  my $sig = "UNK";

  my $hasDigit = ($word =~ /[0-9]/) ? 1 : 0;
  my $hasLower = ($word =~ /[a-z]/) ? 1 : 0;
  my $hasDash  = ($word =~ /\-/)    ? 1 : 0;
  my $numCaps = 0;
  $numCaps++ while $word =~ /[A-Z]/g;

  if ($word =~ /^[A-Z]/) {
    if ($numCaps == 1) {
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
  if ($hasDash) {
    $sig .= "-DASH";
  }

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

# sub classof {
#   my ($word) = @_;

#   $word = delex($word);

#   my $sig = "UNK";
#   my $lowered = lc($word);

#   if ($word =~ /^[A-Z]/) {
#     if (lcfirst($word) eq $lowered) { # only 1st char is uppercase
#       $sig .= "-INITC";
#       $sig .= "-KNOWNLC" if exists $LEXICON->{$lowered};
#     } else { 
#       $sig .= "-CAPS"; 
#     }
#   } else { 
#     if ($word ne $lowered) {
#       $sig .= "-CAPS";
#     } elsif ($word =~ /[A-Z]/) { 
#       $sig .= "-LC"; 
#     } 
#   }

#   $sig .= "-NUM" if ($word =~ /[0-9]/);
#   $sig .= "-DASH" if ($word =~ /[-]/);

#   my $len = length($word);
#   if ($len >= 3 && $lowered =~ /s$/){
#     $sig .= "-s" if !($lowered =~ /ss$/ || $lowered =~ /us$/ || $lowered =~ /is$/);
#   }

#   if ($len >= 5) {
#     $sig .= "-ed", if ($lowered =~ /ed$/);
#     $sig .= "-ing", if ($lowered =~ /ing$/);
#     $sig .= "-ion", if ($lowered =~ /ion$/);
#     $sig .= "-er", if ($lowered =~ /er$/);
#     $sig .= "-est", if ($lowered =~ /est$/);
#     $sig .= "-al", if ($lowered =~ /al$/);
#     if ($lowered =~ /y$/){
#       if ($lowered =~ /ly$/){ 
#         $sig .= "-ly"; 
#       } else { 
#         if ($lowered =~ /ity$/) {  
#           $sig .= "-ity"; 
#         } else { 
#           $sig .= "-y"; 
#         } 
#       }
#     }
#   }
#   return $sig;
# }

# builds a tree structure from a one-line textual representation
sub build_subtree {
  my ($line,$lexicon) = @_;

  $line =~ s/\(/ \(/g;
  $line =~ s/\)/ \) /g;
  my @a = split ' ', $line;
  my $c;
  my @c;
  my $top;
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
        print STDERR "* [$.] FATAL: $c->{label} has no children (token $token)\n";
        exit;
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
      $c = {};
      $c->{label} = $lexicon ? lex(signature($token)) : $token;
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

sub build_subtree_oneline {
  my ($node,$delex) = @_;

  my $str;
  my $numkids = $node->{numkids};
  if ($numkids) {
    $str = "($node->{label}";
    # annotate heads
#     @{$node->{children}}[$node->{hpos}]->{label} =~ s/^/\+/;
    map { $str .= " " . build_subtree_oneline($_,$delex) } @{$node->{children}};
    $str .= ")";
  } else {
    $str = $delex ? delex($node->{label}) : $node->{label};
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

# binarize a single subtree of arbitrary depth
sub binarize_subtree {
  my $args = shift;
  my %defaults = (
    node => undef,  # node to binarize
    not_root => 0,  # true if this is not the root of a tree/subtree
    unique => 1,    # annotate nodes to be part of a subtree
    dir => "right", # left or right binarization
    collapse => "none",  # lhs:put lhs in binarized name
  );
  map { $args->{$_} = $defaults{$_} unless defined $args->{$_} } keys %defaults;

  my $node     = $args->{node};
  my $unique   = $args->{unique};
  my $not_root = $args->{not_root};
  my $dir      = $args->{dir};

  # base case: nothing more to do
  return unless $node->{numkids};

  map { binarize_subtree({node=>$_,not_root=>1,unique=>$unique}) } @{$node->{children}};

  # binarize to a right-branching structure
  while ($node->{numkids} > 2) {
    my $newnode = {};
    if ($dir eq "right") {
      $newnode->{children} = [splice @{$node->{children}},$node->{numkids}-2,2,$newnode];
    } elsif ($dir eq "left") {
      $newnode->{children} = [splice @{$node->{children}},0,2,$newnode];
    }
    my $kidlabels = join ":", map { $_->{label} } @{$newnode->{children}};
#     $newnode->{label} = "<$node->{label}:$kidlabels>";  # :$id
#     $newnode->{label} = "<$kidlabels>";  # :$id

    $newnode->{label} = ($args->{collapse} eq "lhs")
        ? "<$node->{label}:$kidlabels>"
        : "<$kidlabels>";

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

# marks head child of each node
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

# counts terminals in a subtree.
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

sub delex_node {
  my $node = shift;
  $node->{head} = delex($node->{head}) if $node->{head};
}

sub delex_tree {
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

sub is_terminal {
  my ($node) = @_;
  return islex($node->{label});
}

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

# removes internal node marking
sub clean {
  my ($label) = @_;
  $label =~ s/^\*//;

  return $label;
}

# adds a field in each child denoting its parent
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

sub binarize_grammar {
  my ($rulesarg,$collapse) = @_;

  my (%rules,%pmap,%notdone,%counts,%rulemap);

  # 1. count all frontier pairs, and map them to the rule they appear in
  while (my ($rule,$prob) = each %$rulesarg) {
#     print "RULE($rule) $prob\n";
    my ($lhs,@leaves) = split(' ',$rule);
    my $leaves = join(" ",@leaves);
    if (@leaves > 2) {
      $notdone{$lhs}{$leaves} = $prob;
      map { $counts{$lhs}{$leaves[$_-1],$leaves[$_]}++ } (1..$#leaves);
      $rulemap{"$lhs $leaves"} = "$lhs $leaves";
    } else {
      $rules{join($;,@leaves)}{$lhs} = $prob;
    }
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
          my $label = ($collapse eq "lhs")
              ? "<$lhs:$l:$r>"
              : "<$l:$r>";
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
        my $label = ($collapse eq "lhs")
            ? "<$lhs:$l:$r>"
            : "<$l:$r>";

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
  my %binmap;
  foreach my $rule (keys %pmap) {
    # each binary rule points to its parent, and its value is the
    # number of times it appears beneath that parent
    map { $binmap{$_}{$rule} = $pmap{$rule}{$_} } keys %{$pmap{$rule}};
#     map { $binmap{$_}{$rule} = 1.0 } keys %{$pmap{$rule}};
  }

  return (\%rules,\%binmap,\%rulemap);
}

# takes a binarized grammar and pushes the probability mass down as
# far as possible
sub push_weights {
  my ($bin_map,$rules,$rulemap,$method) = @_;

  # 1. sort the LHSs topologically
  my @binrules = sort { bin_level($a) <=> bin_level($b) } keys %$bin_map;
  
  # 2. process each lhs
  foreach my $binrule (@binrules) {
#      print "BINRULE: '$binrule'\n";

    # 2a. determine min prob of top-level rules sharing this bin piece
    my ($lhs,@rhs) = split(' ',$binrule);
    my @tops = keys %{$bin_map->{$binrule}};
    my $shared_prob = 
        max( map { my ($lhs,@rhs) = split ' ', $_;
                   my $rhs = join($;,@rhs);
                   my $numtimes = $bin_map->{$binrule}{$_}; # num times appears
                   if ($method eq "nthroot" and defined $rulemap) {
                     my $fullrule = $rulemap->{join(' ',$lhs,@rhs)};
#                      print "FULLRULE($lhs @rhs) = $fullrule\n";
                     my @nts = split(' ',$fullrule);
                     my $n = @nts - 2;
#                    print "  TOP($_) = $lhs -> $rhs ($rules->{$rhs}{$lhs})\n";
                     $rules->{$rhs}{$lhs} ** (1.0/$numtimes/$n);
                   } else {
                     $rules->{$rhs}{$lhs} ** (1.0/$numtimes);
                   }
               }
             @tops);
#         max( map { my ($lhs,@rhs) = split ' ', $_;
#                    my $rhs = join($;,@rhs);
# #                    print "  TOP($_) = $lhs -> $rhs ($rules->{$rhs}{$lhs})\n";
#                    $rules->{$rhs}{$lhs} } 
#              @tops);

#      print "NEW PROB($binrule) = $shared_prob\n";

    # 2b. assign (some function of) this prob to the bin piece
    my $rhs = join($;,@rhs);
    $rules->{$rhs}{$lhs} = $shared_prob;

    # 2c. divide out that prob from top-level rules sharing this bin piece
    map { my ($lhs,@rhs) = split ' ', $_;
          my $rhs = join($;,@rhs);
          my $numtimes = $bin_map->{$binrule}{$_}; # num times appears
#           print "  PARENT PROB($_) = $rules->{$rhs}{$lhs} / $shared_prob ** $numtimes";
          $rules->{$rhs}{$lhs} /= ($shared_prob ** $numtimes);
#           print " = $rules->{$rhs}{$lhs}\n";
    } @tops;
  }
}

1;
