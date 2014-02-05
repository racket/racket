#;#;
#<<END
TR opt: invalid-exact-inexact.rkt 1:0 (exact->inexact 1.0) -- float to float
END
#<<END
1.0

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port
(exact->inexact 1.0) ; not an integer, can't optimize
