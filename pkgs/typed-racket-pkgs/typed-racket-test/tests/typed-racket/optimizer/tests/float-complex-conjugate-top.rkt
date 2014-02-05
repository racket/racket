#;#;
#<<END
TR opt: float-complex-conjugate-top.rkt 2:0 (conjugate (+ 1.0+2.0i 2.0+4.0i)) -- unboxed unary float complex
TR opt: float-complex-conjugate-top.rkt 2:11 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-conjugate-top.rkt 2:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate-top.rkt 2:23 2.0+4.0i -- unboxed literal
END
#<<END
3.0-6.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(conjugate (+ 1.0+2.0i 2.0+4.0i))
