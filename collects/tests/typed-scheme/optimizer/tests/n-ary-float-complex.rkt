#;
(
n-ary-float-complex.rkt line 15 col 3 - 1.0+2.0i - unboxed literal
n-ary-float-complex.rkt line 15 col 12 - 2.0+4.0i - unboxed literal
n-ary-float-complex.rkt line 15 col 21 - 3.0+6.0i - unboxed literal
n-ary-float-complex.rkt line 15 col 30 - 4.0+8.0i - unboxed literal
n-ary-float-complex.rkt line 15 col 1 - + - unboxed binary float complex
n-ary-float-complex.rkt line 15 col 0 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i) (quote 3.0+6.0i) (quote 4.0+8.0i)) - unboxed float complex
10.0+20.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0+4.0i 3.0+6.0i 4.0+8.0i)
