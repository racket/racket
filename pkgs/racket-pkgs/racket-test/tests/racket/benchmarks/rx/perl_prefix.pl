
use Time::HiRes qw(time);

sub test ($$$) {
  local ($x, $pattern, $times) = @_;

  $start = time;
  for ($i = 0; $i < $times; $i++) {
    $x =~ ${pattern};
  }
  $duration = (time - $start);
  print $duration . "\n";
}
