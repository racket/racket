#;
(
TR opt: float-complex.rkt 18:0 (+ 1.0+2.0i 2.0+4.0i) -- unboxed float complex
TR opt: float-complex.rkt 18:1 + -- unboxed binary float complex
TR opt: float-complex.rkt 18:3 1.0+2.0i -- unboxed literal
TR opt: float-complex.rkt 18:12 2.0+4.0i -- unboxed literal
TR opt: float-complex.rkt 19:0 (- 1.0+2.0i 2.0+4.0i) -- unboxed float complex
TR opt: float-complex.rkt 19:1 - -- unboxed binary float complex
TR opt: float-complex.rkt 19:3 1.0+2.0i -- unboxed literal
TR opt: float-complex.rkt 19:12 2.0+4.0i -- unboxed literal
3.0+6.0i
-1.0-2.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0+4.0i)
(- 1.0+2.0i 2.0+4.0i)
