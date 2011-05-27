#;
(
magnitude.rkt 14:0 (#%app magnitude (quote 3.0+4.0i)) -- unboxed float complex->float
magnitude.rkt 14:1 magnitude -- unboxed unary float complex
magnitude.rkt 14:11 3.0+4.0i -- unboxed literal
5.0
)

#lang typed/racket/base
#:optimize



(magnitude 3.0+4.0i)
