#;
(
TR opt: float-complex-i.rkt 15:0 (+ 1.0+2.0i (* 0+1.0i 2.0+4.0i)) -- unboxed binary float complex
TR opt: float-complex-i.rkt 15:0 (+ 1.0+2.0i (* 0+1.0i 2.0+4.0i)) -- unboxed float complex
TR opt: float-complex-i.rkt 15:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-i.rkt 15:12 (* 0+1.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-i.rkt 15:15 0+1.0i -- unboxed literal
TR opt: float-complex-i.rkt 15:21 2.0+4.0i -- unboxed literal
-3.0+4.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (* +1.0i 2.0+4.0i))
