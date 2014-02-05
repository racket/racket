#;#;
#<<END
TR missed opt: invalid-sqrt.rkt 1:0 (sqrt -2.0) -- unexpected complex type
END
#<<END
0+1.4142135623730951i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(sqrt -2.0) ; not a nonnegative flonum, can't optimize
