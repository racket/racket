
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

test "@{['x' x 100]}",  /(.)*/s, "/(.)*/s", 100000;
test "@{['x' x 1000]}",  /(.)*/s, "/(.)*/s",100000;
test "@{['x' x 10000]}",  /(.)*/s, "/(.)*/s",100000;
test "@{['x' x 100000]}",  /(.)*/s, "/(.)*/s",10000;
test "@{['x' x 100]}",  /.*/m, "/.*/m", 100000;
test "@{['x' x 1000]}",  /.*/m, "/.*/m", 100000;
test "@{['x' x 10000]}",  /.*/m, "/.*/m", 10000;
test "@{['x' x 100000]}",  /.*/m, "/.*/m", 1000;
test "@{['x' x 100]}",  /.*/s, "/.*/s",100000;
test "@{['x' x 1000]}",  /.*/s, "/.*/s",100000;
test "@{['x' x 10000]}",  /.*/s, "/.*/s",100000;
test "@{['x' x 100000]}",  /.*/s, "/.*/s",10000;
test "@{['x' x 100]}",  /x*/, "/x*/", 100000;
test "@{['x' x 1000]}",  /x*/, "/x*/", 100000;
test "@{['x' x 10000]}",  /x*/, "/x*/", 10000;
test "@{['x' x 100000]}",  /x*/, "/x*/", 1000;
test "@{['x' x 100]}",  /[xy]*/, "/[xy]*/", 100000;
test "@{['x' x 1000]}",  /[xy]*/, "/[xy]*/", 10000;
test "@{['x' x 10000]}",  /[xy]*/, "/[xy]*/", 1000;
test "@{['x' x 100000]}",  /[xy]*/, "/[xy]*/", 100;
test "@{['x' x 100]}",  /(.)*/m, "/(.)*/", 100000;
test "@{['x' x 1000]}",  /(.)*/m, "/(.)*/", 100000;
test "@{['x' x 10000]}",  /(.)*/m, "/(.)*/", 10000;
test "@{['x' x 100000]}",  /(.)*/m, "/(.)*/", 1000;
test "@{['x' x 100]}",  /(x)*/, "/(x)*/", 100000;
test "@{['x' x 1000]}",  /(x)*/, "/(x)*/", 100000;
test "@{['x' x 10000]}",  /(x)*/, "/(x)*/", 10000;
test "@{['x' x 100000]}",  /(x)*/, "/(x)*/", 1000;
test "@{['x' x 100]}",  /(y|x)*/, "/(y|x)*/", 10000;
test "@{['x' x 1000]}",  /(y|x)*/, "/(y|x)*/", 1000;
test "@{['x' x 10000]}",  /(y|x)*/, "/(y|x)*/", 100;
test "@{['x' x 100000]}",  /(y|x)*/, "/(y|x)*/", 10;
test "@{['x' x 100]}",  /([yz]|x)*/, "/([yz]|x)*/", 10000;
test "@{['x' x 1000]}",  /([yz]|x)*/, "/([yz]|x)*/", 1000;
test "@{['x' x 10000]}",  /([yz]|x)*/, "/([yz]|x)*/", 100;
test "@{['x' x 100000]}",  /([yz]|x)*/, "/([yz]|x)*/", 10;
test "@{['x' x 100]}",  /([xy])*/, "/([xy])*/", 100000;
test "@{['x' x 1000]}",  /([xy])*/, "/([xy])*/", 10000;
test "@{['x' x 10000]}",  /([xy])*/, "/([xy])*/", 1000;
test "@{['x' x 100000]}",  /([xy])*/, "/([xy])*/", 100;
#test "@{['x' x 100]}",  /((x){2})*/, "/((x){2})*/", 10000;
#test "@{['x' x 1000]}",  /((x){2})*/, "/((x){2})*/", 10000;
#test "@{['x' x 10000]}",  /((x){2})*/, "/((x){2})*/", 1000;
#test "@{['x' x 100000]}",  /((x){2})*/, "/((x){2})*/", 100000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..100)]}FOOBARBAZ",  /[a-z]*FOOBARBAZ/, "/[a-z]*FOOBARBAZ/", 100000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..1000)]}FOOBARBAZ",  /[a-z]*FOOBARBAZ/, "/[a-z]*FOOBARBAZ/", 10000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..10000)]}FOOBARBAZ",  /[a-z]*FOOBARBAZ/, "/[a-z]*FOOBARBAZ/", 1000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..100)]}NOPE",  /[a-z]*FOOBARBAZ/, "/[a-z]*FOOBARBAZ/", 1000000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..1000)]}NOPE",  /[a-z]*FOOBARBAZ/, "/[a-z]*FOOBARBAZ/", 100000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..10000)]}NOPE",  /[a-z]*FOOBARBAZ/, "/[a-z]*FOOBARBAZ/", 10000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..100)]}FOOBARBAZ",  /([a-z])*FOOBARBAZ/, "/([a-z])*FOOBARBAZ/", 100000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..1000)]}FOOBARBAZ",  /([a-z])*FOOBARBAZ/, "/([a-z])*FOOBARBAZ/", 10000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..10000)]}FOOBARBAZ",  /([a-z])*FOOBARBAZ/, "/([a-z])*FOOBARBAZ/", 1000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..100)]}NOPE",  /([a-z])*FOOBARBAZ/, "/([a-z])*FOOBARBAZ/", 1000000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..1000)]}NOPE",  /([a-z])*FOOBARBAZ/, "/([a-z])*FOOBARBAZ/", 100000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..10000)]}NOPE",  /([a-z])*FOOBARBAZ/, "/([a-z])*FOOBARBAZ/", 10000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..100)]}FOOBARBAZ",  /([a-z]|ab)*FOOBARBAZ/, "/([a-z]|ab)*FOOBARBAZ/", 10000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..1000)]}FOOBARBAZ",  /([a-z]|ab)*FOOBARBAZ/, "/([a-z]|ab)*FOOBARBAZ/", 1000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..10000)]}FOOBARBAZ",  /([a-z]|ab)*FOOBARBAZ/, "/([a-z]|ab)*FOOBARBAZ/", 10;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..100)]}NOPE",  /([a-z]|ab)*FOOBARBAZ/, "/([a-z]|ab)*FOOBARBAZ/", 1000000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..1000)]}NOPE",  /([a-z]|ab)*FOOBARBAZ/, "/([a-z]|ab)*FOOBARBAZ/", 100000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..10000)]}NOPE",  /([a-z]|ab)*FOOBARBAZ/, "/([a-z]|ab)*FOOBARBAZ/", 10000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..100)]}NOPE",  /[a-z]*FOOBARBAZ/i, "/[a-z]*FOOBARBAZ/i", 1000;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..1000)]}NOPE",  /[a-z]*FOOBARBAZ/i, "/[a-z]*FOOBARBAZ/i", 10;
test "@{[join undef, map { chr(ord('a') + rand 26) } (1..10000)]}NOPE",  /[a-z]*FOOBARBAZ/i, "/[a-z]*FOOBARBAZ/i", 10;


