#;#;
#<<END
TR opt: float-complex-float.rkt 2:0 (+ 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 2:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 2:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 2:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 3:0 (- 1.0 2.0+4.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 3:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 3:3 1.0 -- float in complex ops
TR opt: float-complex-float.rkt 3:7 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 4:0 (- 1.0+2.0i 2.0 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-float.rkt 4:12 2.0 -- float in complex ops
TR opt: float-complex-float.rkt 4:16 3.0+6.0i -- unboxed literal
TR opt: float-complex-float.rkt 4:3 1.0+2.0i -- unboxed literal
TR opt: float-complex-float.rkt 5:0 (- 1.0+2.0i 2.0+4.0i 3.0) -- unboxed binary float complex
TR opt: float-complex-float.rkt 5:12 2.0+4.0i -- unboxed literal
TR opt: float-complex-float.rkt 5:21 3.0 -- float in complex ops
TR opt: float-complex-float.rkt 5:3 1.0+2.0i -- unboxed literal
END
#<<END
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
