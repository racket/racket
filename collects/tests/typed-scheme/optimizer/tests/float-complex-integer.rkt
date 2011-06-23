#;
(
TR opt: float-complex-integer.rkt 13:0 (+ (expt 2 100) 1.0+2.0i) -- unboxed binary float complex
TR opt: float-complex-integer.rkt 13:0 (+ (expt 2 100) 1.0+2.0i) -- unboxed float complex
TR opt: float-complex-integer.rkt 13:3 (expt 2 100) -- float-arg-expr in complex ops
TR opt: float-complex-integer.rkt 13:16 1.0+2.0i -- unboxed literal
1.2676506002282294e+30+2.0i
)

#lang typed/scheme
#:optimize

(+ (expt 2 100) 1.0+2.0i)
