
use Time::HiRes qw(time);

sub test ($$$$) {
  local ($x, $pattern, $pstr, $times) = @_;

  # print "Trying $pattern $times iterations on " . length($x) . " bytes:\n";

  $start = time;
  for ($i = 0; $i < $times; $i++) {
    $x =~ ${pattern};
  }
  $duration = (time - $start);
  print $duration . "\n";
}
