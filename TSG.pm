package TSG;

use strict;
use Exporter;
use vars qw|@ISA @EXPORT|;

@ISA = qw|Exporter|;
@EXPORT = qw| build_subtree build_subtree_oneline read_lexicon read_pos extract_rules_subtree signature mark_subtree_height count_subtree_lex count_subtree_frontier prune pruneit lex delex islex delex_tree walk walk_postorder frontier lhsof $LEXICON $LEXICON_THRESH ruleof is_terminal is_preterminal process_params scrub_node|;

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
    $prune = ($count == scalar @kids);
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
  $LEXICON_THRESH = $thresh || 2;
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

  return $argword unless $LEXICON;   # no lexicon
  return $argword if ($word =~ /^UNK/);   # word is already a signature
  return $argword if (exists $LEXICON->{$word} and $LEXICON->{$word} >= $LEXICON_THRESH);

  my $sig = "UNK";
  my $lowered = lc($word);

  if ($word =~ /^[A-Z]/) {
    if (lcfirst($word) eq $lowered) { # only 1st char is uppercase
      $sig = "$sig-INITC";
       $sig = "$sig-KNOWNLC", if $LEXICON->{$lowered}==1;
    } else { $sig = "$sig-CAPS"; }
  } else { 
    if ($word ne $lowered) {
      $sig = "$sig-CAPS";
    } else { 
      $sig = "$sig-LC"; } 
  }

  $sig = "$sig-NUM", if ($word =~ /[0-9]/);
  $sig = "$sig-DASH", if ($word =~ /[-]/);

  my $len = length($word);
  if ($len>=3 && $lowered =~ /s$/){
    $sig = "$sig-s", if !($lowered =~ /ss$/ || $lowered =~ /us$/ || $lowered =~ /is$/);
  }

  if ($len>=5) {
    $sig = "$sig-ed", if ($lowered =~ /ed$/);
    $sig = "$sig-ing", if ($lowered =~ /ing$/);
    $sig = "$sig-ion", if ($lowered =~ /ion$/);
    $sig = "$sig-er", if ($lowered =~ /er$/);
    $sig = "$sig-est", if ($lowered =~ /est$/);
    $sig = "$sig-al", if ($lowered =~ /al$/);
    if ($lowered =~ /y$/){
      if ($lowered =~ /ly$/){ 
        $sig = "$sig-ly"; 
      } else { 
        if ($lowered =~ /ity$/) {  
          $sig = "$sig-ity"; 
        } else { 
          $sig = "$sig-y"; 
        } 
      }
    }
  }
  return $sig;
}

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
  my $rule = "$node->{label} -> " . (join " ", map {$_->{label}} @{$node->{children}});
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

1;
