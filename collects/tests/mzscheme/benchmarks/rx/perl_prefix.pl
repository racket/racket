
use Time::HiRes qw(time);

sub test ($$$$) {
  local ($x, $pattern, $pstr, $times) = @_;

  print "$pstr $times iterations on " . length($x) . " bytes:\n";

  $start = time;
  for ($i = 0; $i < $times; $i++) {
    $x =~ ${pattern};
  }
  print (time - $start);
    print "\n";
}
