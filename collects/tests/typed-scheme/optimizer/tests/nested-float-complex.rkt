#;
(
TR opt: nested-float-complex.rkt 14:0 (+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i)) -- unboxed binary float complex
TR opt: nested-float-complex.rkt 14:3 1.0+2.0i -- unboxed literal
TR opt: nested-float-complex.rkt 14:12 (- 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: nested-float-complex.rkt 14:15 2.0+4.0i -- unboxed literal
TR opt: nested-float-complex.rkt 14:24 3.0+6.0i -- unboxed literal
0.0+0.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i))
