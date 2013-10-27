#;#;
#<<END
TR opt: float-complex-float-small.rkt 35:0 (+ 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 35:12 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 35:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 36:0 (+ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 36:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 36:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 37:0 (- 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 37:12 3.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 37:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 38:0 (- 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 38:3 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 38:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 39:0 (+ 1.0+2.0i (+ 1.0 2.0)) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 39:12 (+ 1.0 2.0) -- binary float
TR opt: float-complex-float-small.rkt 39:12 (+ 1.0 2.0) -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 39:15 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 39:15 1.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 39:19 2.0 -- float-arg-expr in complex ops
TR opt: float-complex-float-small.rkt 39:3 1.0+2.0i -- unboxed literal
END
#<<END
4.0+2.0i
3.0+4.0i
-2.0+2.0i
-1.0-4.0i
4.0+2.0i

END

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 3.0)
(+ 1.0 2.0+4.0i)
(- 1.0+2.0i 3.0)
(- 1.0 2.0+4.0i)
(+ 1.0+2.0i (+ 1.0 2.0))
