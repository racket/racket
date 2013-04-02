#;
(
TR opt: complex-exp.rkt 12:8 0 -- float-arg-expr in complex ops
TR opt: complex-exp.rkt 12:3 (exp 0) -- unboxed unary float complex
TR opt: complex-exp.rkt 12:11 1.0+2.0i -- unboxed literal
TR opt: complex-exp.rkt 12:0 (- (exp 0) 1.0+2.0i) -- unboxed binary float complex
0.0-2.0i
)

#lang typed/racket

(- (exp 0) 1.0+2.0i)
