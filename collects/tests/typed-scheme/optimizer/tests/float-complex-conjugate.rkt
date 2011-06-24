#;
(
TR opt: float-complex-conjugate.rkt 14:0 (+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i)) -- unboxed binary float complex
TR opt: float-complex-conjugate.rkt 14:3 (conjugate 1.0+2.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 14:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 14:24 (conjugate 2.0+4.0i) -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 14:35 2.0+4.0i -- unboxed literal
3.0-6.0i
)

#lang typed/scheme
#:optimize

(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
