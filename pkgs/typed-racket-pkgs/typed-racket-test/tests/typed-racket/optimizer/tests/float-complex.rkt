#;#;
#<<END
TR opt: float-complex.rkt 2:0 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex.rkt 2:12 2.0+4.0i -- unboxed literal
TR opt: float-complex.rkt 2:3 1.0+2.0i -- unboxed literal
TR opt: float-complex.rkt 3:0 (- 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: float-complex.rkt 3:12 2.0+4.0i -- unboxed literal
TR opt: float-complex.rkt 3:3 1.0+2.0i -- unboxed literal
END
#<<END
3.0+6.0i
-1.0-2.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ 1.0+2.0i 2.0+4.0i)
(- 1.0+2.0i 2.0+4.0i)
