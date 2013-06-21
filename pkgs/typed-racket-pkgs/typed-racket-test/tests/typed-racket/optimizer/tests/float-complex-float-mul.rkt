#;#;
#<<END
TR opt: float-complex-float-mul.rkt 39:0 (* 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 39:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 39:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 40:0 (* 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 40:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 40:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 41:0 (* 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 41:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 41:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 41:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 42:0 (* 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 42:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 42:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 42:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 43:0 (* 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 43:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 43:21 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 43:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 44:0 (* 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 44:12 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 44:16 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-mul.rkt 44:3 1.0+2.0i -- unboxed literal
END
#<<END
2.0+4.0i
2.0+4.0i
-18.0+24.0i
-18.0+24.0i
-18.0+24.0i
6.0+12.0i

END

#lang typed/scheme
#:optimize

(* 1.0 2.0+4.0i)
(* 1.0+2.0i 2.0)
(* 1.0 2.0+4.0i 3.0+6.0i)
(* 1.0+2.0i 2.0 3.0+6.0i)
(* 1.0+2.0i 2.0+4.0i 3.0)
(* 1.0+2.0i 2.0 3.0)
