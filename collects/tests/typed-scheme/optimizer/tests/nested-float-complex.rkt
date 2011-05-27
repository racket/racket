#;
(
nested-float-complex.rkt 15:0 (#%app + (quote 1.0+2.0i) (#%app - (quote 2.0+4.0i) (quote 3.0+6.0i))) -- unboxed float complex
nested-float-complex.rkt 15:1 + -- unboxed binary float complex
nested-float-complex.rkt 15:3 1.0+2.0i -- unboxed literal
nested-float-complex.rkt 15:13 - -- unboxed binary float complex
nested-float-complex.rkt 15:15 2.0+4.0i -- unboxed literal
nested-float-complex.rkt 15:24 3.0+6.0i -- unboxed literal
0.0+0.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i))
