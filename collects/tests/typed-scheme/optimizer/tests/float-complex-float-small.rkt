#;
(
TR opt: float-complex-float-small.rkt 31:0 (+ 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 31:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 31:12 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 32:0 (+ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 32:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 32:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 33:0 (- 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 33:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 33:12 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 34:0 (- 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 34:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 34:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 35:0 (+ 1.0+2.0i (+ 1.0 2.0)) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 35:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 35:12 (+ 1.0 2.0) -- binary float
TR opt: float-complex-float-small.rkt 35:12 (+ 1.0 2.0) -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 35:15 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 35:19 2.0 -- float-arg-expr in complex ops
4.0+2.0i
3.0+4.0i
-2.0+2.0i
-1.0-4.0i
4.0+2.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 3.0)
(+ 1.0 2.0+4.0i)
(- 1.0+2.0i 3.0)
(- 1.0 2.0+4.0i)
(+ 1.0+2.0i (+ 1.0 2.0))
