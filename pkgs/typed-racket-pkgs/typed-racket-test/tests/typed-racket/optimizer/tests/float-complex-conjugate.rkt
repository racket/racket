#;#;
#<<END
TR opt: float-complex-conjugate.rkt 17:0 (+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i)) -- unboxed binary float complex
TR opt: float-complex-conjugate.rkt 17:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 17:24 (conjugate 2.0+4.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 17:3 (conjugate 1.0+2.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 17:35 2.0+4.0i -- unboxed literal
END
#<<END
3.0-6.0i

END

#lang typed/scheme
#:optimize

(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
