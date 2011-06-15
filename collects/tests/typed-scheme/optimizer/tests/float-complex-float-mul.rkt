#;
(
TR opt: float-complex-float-mul.rkt 42:0 (* 1.0 2.0+4.0i) -- unboxed float complex
TR opt: float-complex-float-mul.rkt 42:1 * -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 42:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 42:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 43:0 (* 1.0+2.0i 2.0) -- unboxed float complex
TR opt: float-complex-float-mul.rkt 43:1 * -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 43:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 43:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 44:0 (* 1.0 2.0+4.0i 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float-mul.rkt 44:1 * -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 44:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 44:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 44:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 45:0 (* 1.0+2.0i 2.0 3.0+6.0i) -- unboxed float complex
TR opt: float-complex-float-mul.rkt 45:1 * -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 45:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 45:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 45:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 46:0 (* 1.0+2.0i 2.0+4.0i 3.0) -- unboxed float complex
TR opt: float-complex-float-mul.rkt 46:1 * -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 46:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 46:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 46:21 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 47:0 (* 1.0+2.0i 2.0 3.0) -- unboxed float complex
TR opt: float-complex-float-mul.rkt 47:1 * -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 47:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 47:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 47:16 3.0 -- float-arg-expr in complex ops
2.0+4.0i
2.0+4.0i
-18.0+24.0i
-18.0+24.0i
-18.0+24.0i
6.0+12.0i
)

#lang typed/scheme
#:optimize

(* 1.0 2.0+4.0i)
(* 1.0+2.0i 2.0)
(* 1.0 2.0+4.0i 3.0+6.0i)
(* 1.0+2.0i 2.0 3.0+6.0i)
(* 1.0+2.0i 2.0+4.0i 3.0)
(* 1.0+2.0i 2.0 3.0)
