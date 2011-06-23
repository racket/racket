#;
(
TR opt: float-complex-float-small.rkt 36:0 (+ 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 36:0 (+ 1.0+2.0i 3.0) -- unboxed float complex
TR opt: float-complex-float-small.rkt 36:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 36:12 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 37:0 (+ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 37:0 (+ 1.0 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-float-small.rkt 37:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 37:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 38:0 (- 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 38:0 (- 1.0+2.0i 3.0) -- unboxed float complex
TR opt: float-complex-float-small.rkt 38:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 38:12 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 39:0 (- 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 39:0 (- 1.0 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-float-small.rkt 39:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 39:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 40:0 (+ 1.0+2.0i (+ 1.0 2.0)) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 40:0 (+ 1.0+2.0i (+ 1.0 2.0)) -- unboxed float complex
TR opt: float-complex-float-small.rkt 40:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 40:12 (+ 1.0 2.0) -- binary float
TR opt: float-complex-float-small.rkt 40:12 (+ 1.0 2.0) -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 40:15 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 40:19 2.0 -- float-arg-expr in complex ops
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
