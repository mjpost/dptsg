#! /usr/bin/perl

# puts treebank parse each on one line
# same effect as "tgrep -wn '/.*/' | grep -v ^$"

# fixed bug that skips parses beginning with (( instead of ( (

while (<>) {
  next if /^\*/;
  s/\(\(/( (/g;  # some lines start with (( instead of ( (
  s/\s+/ /g;
  if (/^\(/ && $line) {
    print_line($line);
  }
  chop;
  $line .= $_;
}
print_line($line);

sub print_line {
  $line = shift;
  $line =~ s/ +/ /g;
  $line =~ s/^\( /(TOP /g;
  print "$line\n";
  $line = '';
}
