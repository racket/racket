#;
(
magnitude.rkt line 14 col 11 - 3.0+4.0i - unboxed literal
magnitude.rkt line 14 col 1 - magnitude - unboxed unary float complex
magnitude.rkt line 14 col 0 - (#%app magnitude (quote 3.0+4.0i)) - unboxed float complex->float
5.0
)

#lang typed/racket/base
#:optimize



(magnitude 3.0+4.0i)
