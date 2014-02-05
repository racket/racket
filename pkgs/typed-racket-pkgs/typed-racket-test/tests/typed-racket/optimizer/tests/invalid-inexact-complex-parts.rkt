#;#;
#<<END
TR info: invalid-inexact-complex-parts.rkt 1:0 (real-part 1+2i) -- possible exact real arith
END
#<<END
1

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(real-part 1+2i)
