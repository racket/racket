#;
(
TR opt: float-complex-float.rkt 32:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float.rkt 32:1 + -- unboxed binary float complex
TR opt: float-complex-float.rkt 32:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 32:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float.rkt 32:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 33:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float.rkt 33:1 - -- unboxed binary float complex
TR opt: float-complex-float.rkt 33:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float.rkt 33:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 33:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 34:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float.rkt 34:1 - -- unboxed binary float complex
TR opt: float-complex-float.rkt 34:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 34:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float.rkt 34:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 35:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed float complex
TR opt: float-complex-float.rkt 35:1 - -- unboxed binary float complex
TR opt: float-complex-float.rkt 35:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 35:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 35:21 3.0 -- float-arg-expr in complex ops
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
