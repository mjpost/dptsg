#!/usr/bin/perl


my $basedir;
BEGIN {
  $basedir = "$ENV{HOME}/code/dpinfer";
  unshift @INC, $basedir;
}

use List::Util;
use TSG;

# parameters (via environment variables and command-line params)
my %PARAMS = (
  lexicon => "$basedir/data/lex.02-21",
  rundir => $ENV{PWD},
  srand => undef,
  tagged => 0,
  verbosity => 1 );

process_params(\%PARAMS,\@ARGV,\%ENV);
my $lexicon = read_lexicon($PARAMS{lexicon},$PARAMS{thresh});

# initialize the random number generator if requested
srand($PARAMS{srand}) if defined $PARAMS{srand};

my $shuffled = 0;
while (my $line = <>) {
  chomp($line);

  my $subtree = build_subtree($line,$lexicon);

  walk($subtree,[\&shuffle_NPs]);

  if ($PARAMS{tagged}) {
    my $sent = frontier_preterm($subtree);
    my @sent = split(" ", $sent);
    my $numwords = scalar @sent;
    $numwords /= 2;
    print "$numwords $sent\n";
  } else {
    print frontier($subtree), $/;
  }
}

sub shuffle {
  my ($node) = @_;

  return if $shuffled > $PARAMS{max_shuffle};

  my $numkids = scalar @{$node->{children}};

  if ($numkids) {
    my @kids = @{$node->{children}};

    if ($kids[-1]->{label} eq ".") {
      my $last = pop @kids;
      @kids = (List::Util::shuffle(@kids),$last);
    } else {
      @kids = List::Util::shuffle(@kids);
    }    

    $node->{children} = \@kids;
  }
}

sub shuffle_NPs {
  my ($node) = @_;

  my $numkids = scalar @{$node->{children}};
  if ($numkids and $node->{label} eq "NP") {
    @{$node->{children}} = reverse @{$node->{children}};
  }
}

sub frontier_preterm {
  my ($node) = @_;

  if (is_preterminal($node)) {
    my $kid = @{$node->{children}}[0];
    my $lab = delex($node->{label});
    my $kidlab = delex($kid->{label});
    return "$kidlab $lab";
  } else {
    return join(" ", map { frontier_preterm($_) } @{$node->{children}});
  }
}
