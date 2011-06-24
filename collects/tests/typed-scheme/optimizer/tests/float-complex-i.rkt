#;
(
TR opt: float-complex-i.rkt 14:0 (+ 1.0+2.0i (* 0+1.0i 2.0+4.0i)) -- unboxed binary float complex
TR opt: float-complex-i.rkt 14:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-i.rkt 14:12 (* 0+1.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-i.rkt 14:15 0+1.0i -- unboxed literal
TR opt: float-complex-i.rkt 14:21 2.0+4.0i -- unboxed literal
-3.0+4.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i (* +1.0i 2.0+4.0i))
