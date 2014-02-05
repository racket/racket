#;#;
#<<END
TR opt: float-complex-float-mul.rkt 2:0 (* 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 2:3 1.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 2:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 3:0 (* 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 3:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 3:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 4:0 (* 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 4:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 4:3 1.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 4:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 5:0 (* 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 5:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 5:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 5:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 6:0 (* 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 6:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 6:21 3.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 6:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 7:0 (* 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 7:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 7:16 3.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 7:3 1.0+2.0i -- unboxed literal
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
#reader tests/typed-racket/optimizer/reset-port

(* 1.0 2.0+4.0i)
(* 1.0+2.0i 2.0)
(* 1.0 2.0+4.0i 3.0+6.0i)
(* 1.0+2.0i 2.0 3.0+6.0i)
(* 1.0+2.0i 2.0+4.0i 3.0)
(* 1.0+2.0i 2.0 3.0)
