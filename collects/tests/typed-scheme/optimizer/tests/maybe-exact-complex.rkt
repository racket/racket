#;
(
maybe-exact-complex.rkt line 15 col 3 - 1.0+2.0i - unboxed literal
maybe-exact-complex.rkt line 15 col 12 - 2+4i - unboxed literal
maybe-exact-complex.rkt line 15 col 1 - + - unboxed binary float complex
maybe-exact-complex.rkt line 15 col 0 - (#%app + (quote 1.0+2.0i) (quote 2+4i)) - unboxed float complex
3.0+6.0i
)

#lang typed/scheme
#:optimize



(+ 1.0+2.0i 2+4i)
