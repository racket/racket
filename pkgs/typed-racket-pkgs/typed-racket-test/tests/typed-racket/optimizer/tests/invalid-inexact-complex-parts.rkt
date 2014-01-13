#;#;
#<<END
TR info: invalid-inexact-complex-parts.rkt 12:0 (real-part 1+2i) -- exact real arith
END
#<<END
1

END

#lang typed/scheme
#:optimize
(real-part 1+2i)
