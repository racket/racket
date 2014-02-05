#;#;
#<<END
TR opt: float-complex-float.rkt 51:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 51:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 51:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 51:16 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float.rkt 51:16 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 51:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 51:3 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float.rkt 51:3 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 51:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 52:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 52:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 52:16 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float.rkt 52:16 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 52:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 52:3 1.0 -- float in complex ops
TR opt: float-complex-float.rkt 52:7 2.0+4.0i -- leave var unboxed
TR opt: float-complex-float.rkt 52:7 2.0+4.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 52:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 53:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 53:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 53:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 53:16 3.0+6.0i -- leave var unboxed
TR opt: float-complex-float.rkt 53:16 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 53:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 53:3 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float.rkt 53:3 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 53:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 54:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 54:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 54:12 2.0+4.0i -- leave var unboxed
TR opt: float-complex-float.rkt 54:12 2.0+4.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 54:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 54:21 3.0 -- float in complex ops
TR opt: float-complex-float.rkt 54:3 1.0+2.0i -- leave var unboxed
TR opt: float-complex-float.rkt 54:3 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-float.rkt 54:3 1.0+2.0i -- unboxed literal
END
#<<END
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
