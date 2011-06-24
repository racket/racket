#;
(
TR opt: magnitude.rkt 13:0 (magnitude 3.0+4.0i) -- unboxed unary float complex
TR opt: magnitude.rkt 13:11 3.0+4.0i -- unboxed literal
5.0
)

#lang typed/racket/base
#:optimize



(magnitude 3.0+4.0i)
