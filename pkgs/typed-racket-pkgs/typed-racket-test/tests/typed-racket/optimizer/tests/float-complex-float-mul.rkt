#;#;
#<<END
TR opt: float-complex-float-mul.rkt 57:0 (* 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 57:3 1.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 57:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 58:0 (* 1.0+2.0i 2.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 58:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 58:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 59:0 (* 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 59:0 (* 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 59:16 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float-mul.rkt 59:16 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float-mul.rkt 59:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 59:3 1.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 59:7 2.0+4.0i -- leave var unboxed
TR opt: float-complex-float-mul.rkt 59:7 2.0+4.0i -- unboxed let bindings
TR opt: float-complex-float-mul.rkt 59:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 60:0 (* 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 60:0 (* 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 60:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 60:16 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float-mul.rkt 60:16 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float-mul.rkt 60:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 60:3 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float-mul.rkt 60:3 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float-mul.rkt 60:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 61:0 (* 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 61:0 (* 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 61:12 2.0+4.0i -- leave var unboxed
TR opt: float-complex-float-mul.rkt 61:12 2.0+4.0i -- unboxed let bindings
TR opt: float-complex-float-mul.rkt 61:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 61:21 3.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 61:3 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float-mul.rkt 61:3 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float-mul.rkt 61:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-mul.rkt 62:0 (* 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 62:0 (* 1.0+2.0i 2.0 3.0) -- unboxed binary float complex
TR opt: float-complex-float-mul.rkt 62:12 2.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 62:16 3.0 -- float in complex ops
TR opt: float-complex-float-mul.rkt 62:3 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float-mul.rkt 62:3 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float-mul.rkt 62:3 1.0+2.0i -- unboxed literal
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
