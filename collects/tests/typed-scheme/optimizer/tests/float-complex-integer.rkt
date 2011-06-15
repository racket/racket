#;
(
TR opt: float-complex-integer.rkt 13:0 (#%app + (#%app expt (quote 2) (quote 100)) (quote 1.0+2.0i)) -- unboxed float complex
TR opt: float-complex-integer.rkt 13:1 + -- unboxed binary float complex
TR opt: float-complex-integer.rkt 13:3 (#%app expt (quote 2) (quote 100)) -- float-arg-expr in complex ops
TR opt: float-complex-integer.rkt 13:16 1.0+2.0i -- unboxed literal
1.2676506002282294e+30+2.0i
)

#lang typed/scheme
#:optimize

(+ (expt 2 100) 1.0+2.0i)
