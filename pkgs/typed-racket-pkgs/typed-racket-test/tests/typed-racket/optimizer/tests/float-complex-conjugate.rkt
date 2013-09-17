#;#;
#<<END
TR opt: float-complex-conjugate.rkt 20:0 (+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i)) -- unboxed binary float complex
TR opt: float-complex-conjugate.rkt 20:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 20:24 (conjugate 2.0+4.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 20:3 (conjugate 1.0+2.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 20:35 2.0+4.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 21:0 (conjugate 1.0+0.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 21:11 1.0+0.0i -- unboxed literal
END
#<<END
3.0-6.0i
1.0-0.0i

END

#lang typed/scheme
#:optimize

(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
(conjugate 1.0+0.0i)
