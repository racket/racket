#;
(
TR opt: float-complex-conjugate.rkt 15:0 (#%app + (#%app conjugate (quote 1.0+2.0i)) (#%app conjugate (quote 2.0+4.0i))) -- unboxed float complex
TR opt: float-complex-conjugate.rkt 15:1 + -- unboxed binary float complex
TR opt: float-complex-conjugate.rkt 15:4 conjugate -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 15:14 1.0+2.0i -- unboxed literal
TR opt: float-complex-conjugate.rkt 15:25 conjugate -- unboxed unary float complex
TR opt: float-complex-conjugate.rkt 15:35 2.0+4.0i -- unboxed literal
3.0-6.0i
)

#lang typed/scheme
#:optimize

(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
