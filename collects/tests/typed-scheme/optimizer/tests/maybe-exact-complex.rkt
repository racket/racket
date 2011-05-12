#;
(
maybe-exact-complex.rkt 15:3 1.0+2.0i -- unboxed literal
maybe-exact-complex.rkt 15:12 2+4i -- unboxed literal
maybe-exact-complex.rkt 15:1 + -- unboxed binary float complex
maybe-exact-complex.rkt 15:0 (#%app + (quote 1.0+2.0i) (quote 2+4i)) -- unboxed float complex
3.0+6.0i
)

#lang typed/scheme
#:optimize



(+ 1.0+2.0i 2+4i)
