#;#;
#<<END
TR opt: float-complex-float-small.rkt 2:0 (+ 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 2:12 3.0 -- float in complex ops
TR opt: float-complex-float-small.rkt 2:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 3:0 (+ 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 3:3 1.0 -- float in complex ops
TR opt: float-complex-float-small.rkt 3:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 4:0 (- 1.0+2.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 4:12 3.0 -- float in complex ops
TR opt: float-complex-float-small.rkt 4:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 5:0 (- 1.0 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 5:3 1.0 -- float in complex ops
TR opt: float-complex-float-small.rkt 5:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float-small.rkt 6:0 (+ 1.0+2.0i (+ 1.0 2.0)) -- unboxed binary float complex
TR opt: float-complex-float-small.rkt 6:12 (+ 1.0 2.0) -- binary float
TR opt: float-complex-float-small.rkt 6:12 (+ 1.0 2.0) -- float in complex ops
TR opt: float-complex-float-small.rkt 6:15 1.0 -- float in complex ops
TR opt: float-complex-float-small.rkt 6:15 1.0 -- float in complex ops
TR opt: float-complex-float-small.rkt 6:19 2.0 -- float in complex ops
TR opt: float-complex-float-small.rkt 6:3 1.0+2.0i -- unboxed literal
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
#reader tests/typed-racket/optimizer/reset-port

(+ 1.0+2.0i 3.0)
(+ 1.0 2.0+4.0i)
(- 1.0+2.0i 3.0)
(- 1.0 2.0+4.0i)
(+ 1.0+2.0i (+ 1.0 2.0))
