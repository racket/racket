#;
(
TR opt: maybe-exact-complex.rkt 14:3 1.0+2.0i -- unboxed literal
TR opt: maybe-exact-complex.rkt 14:12 2+4i -- unboxed literal
TR opt: maybe-exact-complex.rkt 14:0 (+ 1.0+2.0i 2+4i) -- unboxed binary float complex
3.0+6.0i
)

#lang typed/scheme
#:optimize



(+ 1.0+2.0i 2+4i)
