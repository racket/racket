#;#;
#<<END
TR opt: float-complex-conjugate.rkt 2:0 (+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i)) -- unboxed binary float complex
TR opt: float-complex-conjugate.rkt 2:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 2:24 (conjugate 2.0+4.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 2:3 (conjugate 1.0+2.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 2:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 3:0 (conjugate 1.0+0.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 3:11 1.0+0.0i -- unboxed literal
END
#<<END
3.0-6.0i
1.0-0.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
(conjugate 1.0+0.0i)
