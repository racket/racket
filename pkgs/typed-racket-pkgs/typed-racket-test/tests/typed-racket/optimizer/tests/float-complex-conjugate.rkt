#;#;
#<<END
TR opt: float-complex-conjugate.rkt 18:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 18:3 (conjugate 1.0+2.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 18:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 18:24 (conjugate 2.0+4.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 18:0 (+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i)) -- unboxed binary float complex

END
#<<END
3.0-6.0i

END

#lang typed/scheme
#:optimize

(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
