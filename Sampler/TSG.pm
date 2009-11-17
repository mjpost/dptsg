package Sampler::TSG;

use strict;
use Sampler;
use Exporter;
use vars qw|$VERSION @ISA @EXPORT @EXPORT_OK %EXPORT_TAGS|;

# our($lexicon,%rules,%deps,%PARAMS,$base_measure);

@ISA = qw|Exporter Sampler|;
@EXPORT = qw|sample_each_TSG|;
@EXPORT_OK = ();

use strict;
use warnings;
use threads;
use threads::shared;
use POSIX qw|strftime|;
use List::Util qw|reduce min shuffle sum|;
# use Memoize;

my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use TSG;

sub new {
  my ($class,@rest) = @_;

  my $self = $class->SUPER::new(@rest);

  $self->{alpha} = 1;        # default alpha
  $self->{totals} = {};
  $self->{rewrites} = {};   # counts of lhs -> {N U T}* (a string of terms and nts)

  bless($self,$class);

  return $self;
}

# counts all of the rules in a tree by updating $hash
# will also update a counter ($size reference) if available
sub count {
  my ($self) = @_;
  my $corpus = $self->{corpus};

  $self->{totals} = {};
  $self->{rewrites} = {};

  my $count = sub {
    my ($node) = @_;
    my $lhs = $node->{label};
    if (@{$node->{children}} && $lhs !~ /^\*/) {
      my $rule = ruleof($node);
      $self->{rewrites}{$lhs}{$rule}++;
      $self->{totals}{$lhs}++;
    }
  };

  # count all the subtrees in the corpus
  map { walk($_,[$count]) } @$corpus;
}

my $debug = 0;
my $loghandle;

sub sample_each_TSG {
  my ($node,$self,$topnode) = @_;

  return $node if $node->{label} eq "TOP";
  return $node unless @{$node->{children}};

  # print "SAMPLE_EACH_TSG($node->{label},$topnode->{label})\n";

  # base case: can't split leaves
  my $numkids = scalar @{$node->{children}};

  my ($inside_str,$outside_str,$merged_str);

  # is merged
  my $was_merged = ($node->{label} =~ /^\*/) ? 1 : 0;
  if ($was_merged) {
    $merged_str = ruleof($topnode);
    $node->{label} =~ s/^\*//;
    $outside_str = ruleof($topnode);
    $inside_str = ruleof($node);

    decrement($self->{rewrites}{lhsof($merged_str)},$merged_str);
    decrement($self->{totals},lhsof($merged_str));
  } else {
    $outside_str = ruleof($topnode);
    $inside_str = ruleof($node);
    $node->{label} = '*' . $node->{label};
    $merged_str = ruleof($topnode);

    decrement($self->{rewrites}{lhsof($inside_str)},$inside_str);
    decrement($self->{rewrites}{lhsof($outside_str)},$outside_str);
    decrement($self->{totals},lhsof($inside_str));
    decrement($self->{totals},lhsof($outside_str));
  }

  # compute the three items with a combination of merging and removing
  # annotation
  if ($debug) {
    print "--\n";
    print "OUTSIDE: $outside_str\n";
    print "INSIDE:  $inside_str\n";
    print "MERGED: $merged_str\n";
  }

  # # set counts of lhs to 0 (if not set) to prevent later "undefined" notices
  # map { $self->{size}->{lhsof($_)} = 0 unless exists $self->{size}->{lhsof($_)} } ($outside_str, $inside_str, $merged_str);

  # compute relative probability of merged vs. inside + outside
  my $prob_merged = $self->prob($merged_str);
  my $prob_inside = $self->prob($inside_str);
  my $prob_outside = $self->prob($outside_str);
  my $denom = $prob_merged + $prob_inside * $prob_outside;

  # if ($denom <= 0) {
  #   print "\n--\nCONSIDERING NODE: $tree->{label} -> ", (join " ", (map { $_->{label} } @{$tree->{children}})), $/;

  #   print "-- FAIL REPORT\n";
  #   print "  was_merged = $was_merged\n";
  #   print "  OUTSIDE: $outside->{str} ($outside->{rulecount} rules) (", $self->prob($outside,1), ")\n";
  #   print "  INSIDE:  $inside->{str} ($inside->{rulecount} rules) (", $self->prob($inside,1), ")\n";
  #   print "  MERGED: $merged->{str} ($merged->{rulecount} rules) (", $self->prob($merged,1), ")\n";
  # }

  my $merge_prob = $denom ? ($prob_merged / $denom) : 0;

  # transition with that possibility
  my $do_merge = rand($prob_merged + $prob_inside * $prob_outside) < $prob_merged;

#   print "MERGE $merge_prob (merging=$merging)\n";
#   print "  - $merged_str ($prob_merged)\n";
#   print "  - $outside_str ($prob_outside)\n";
#   print "  - $inside_str ($prob_inside)\n";

#   print "merged to get: '$merged->{str}' [$merge_prob] ";
  print "CHOSE $merged_str ($merge_prob)\n" if $do_merge and $debug;
#   print $/;

  # if ($self->{log}) {
  #   my $fh = $self->{log};
  #   print $fh "$self->{treeno} $gorn $merge_prob $co $ci $cm $was_merged $merging\n";
  # }

  if ($do_merge) {
#     print "  ADDING MERGED $merged_str\n";
    my $lhs = lhsof($merged_str);
    $self->{rewrites}{$lhs}{$merged_str}++;
    $self->{totals}{$lhs}++;
    # we need to add the asterisk, which not there now if it was there before
    $node->{label} = "*" . $node->{label} if $was_merged;
  } else {
    my $olhs = lhsof($outside_str);
    my $ilhs = lhsof($inside_str);

    # print "  ADDING OUTSIDE $outside_str ($olhs)\n";
    # print "  ADDING INSIDE $inside_str ($ilhs)\n";

    $self->{rewrites}{$olhs}{$outside_str}++;
    $self->{rewrites}{$ilhs}{$inside_str}++;
    $self->{totals}{$olhs}++;
    $self->{totals}{$ilhs}++;
    # we need to clear the asterisk, which is there now if it wasn't
    # there before
    $node->{label} =~ s/^\*// unless $was_merged;
  }
  
  # If we merged (or stayed merged), the same topnode will continue to
  # be the topnode.  If we are not merged, then the current node is
  # the root of (potential) trees below it
  return $topnode if ($do_merge);
  return $node;
}

sub prob {
  my ($self,$rule,$verb) = @_;

  my $subtree = build_subtree($rule);
  my $lhs = $subtree->{label};

  my $count = (exists $self->{rewrites}{$rule}) 
      ? $self->{rewrites}{$rule} : 0;

  my $total = (exists $self->{totals}{$lhs})
      ? $self->{totals}{$lhs} : 0;

  my $num = $count + $self->{alpha} * $self->base_prob($self,$subtree);
  my $denom = $total + $self->{alpha};
  
  # print "UNDEF($lhs) TOTALS\n" unless defined $self->{totals}->{$lhs};
  # print "UNDEF($rule,$lhs) ALPHA\n" unless defined $self->{alpha};

  # if ($verb) {
  #   print "PROB($rule)\n";
  #   print "  counts = $count\n";
  #   print "  alpha = $self->{alpha}\n";
  #   print "  base_prob = ", $self->base_prob($self,$subtree), $/;
  #   print "  size = $self->{totals}->{$lhs}\n";
  #   print "  ", $self->base_prob($self,$subtree), $/;
  #   print "  NUM = $num\n";
  #   print "  DEN = $denom\n";
  # }

  my $prob = $num / $denom;

  return $prob;
}

# returns the base measure probability of the tree fragment
# memoize('base_prob');
sub base_prob {
  my ($self,$subtree,$verb) = @_;

  my $pr = 1.0;
#   print "BASE_PROB: ", (scalar @{$rep->{rules}}), " rules:\n"
#       if $debug;

  my @rules;
  extract_rules_subtree($subtree,\@rules);
  my $numrules = scalar @rules;

  foreach my $rule (@rules) {
    print " PROB($rule) = $self->{rules}->{$rule}\n"
        if $debug;
    print "* WARNING: couldn't find rule '$rule' in PCFG rules\n" unless exists $self->{rules}->{$rule};
    $pr *= $self->{rules}->{$rule};
  }
#   print "PR is $pr after multiplying together $rep->{rulecount} rules\n";

  my $prg = ((1.0 - $self->{stop}) ** ($numrules - 1)) * $self->{stop};

  if ($verb) {
    my $ps = $self->{stop};
    my $ps2 = 1.0 - $ps;
    my $rules = $numrules - 1;
    # print "BASE_PROB() = $pr * $prg ($ps2 ** $numrules * $ps) = ", $pr * $prg,$/;
  }

  return $pr * $prg;
}

1;

