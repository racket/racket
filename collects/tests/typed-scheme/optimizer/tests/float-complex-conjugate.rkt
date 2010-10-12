#;
(
float-complex-conjugate.rkt line 15 col 14 - 1.0+2.0i - unboxed literal
float-complex-conjugate.rkt line 15 col 4 - conjugate - unboxed unary float complex
float-complex-conjugate.rkt line 15 col 35 - 2.0+4.0i - unboxed literal
float-complex-conjugate.rkt line 15 col 25 - conjugate - unboxed unary float complex
float-complex-conjugate.rkt line 15 col 1 - + - unboxed binary float complex
float-complex-conjugate.rkt line 15 col 0 - (#%app + (#%app conjugate (quote 1.0+2.0i)) (#%app conjugate (quote 2.0+4.0i))) - unboxed float complex
3.0-6.0i
)

#lang typed/scheme
#:optimize

(+ (conjugate 1.0+2.0i) (conjugate 2.0+4.0i))
