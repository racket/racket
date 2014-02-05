#;#;
#<<END
TR opt: float-complex-i.rkt 2:0 (+ 1.0+2.0i (* 0+1.0i 2.0+4.0i)) -- unboxed binary float complex
TR opt: float-complex-i.rkt 2:12 (* 0+1.0i 2.0+4.0i) -- non float complex in complex ops
TR opt: float-complex-i.rkt 2:15 0+1.0i -- unboxed literal
TR opt: float-complex-i.rkt 2:15 0+1.0i -- unboxed literal
TR opt: float-complex-i.rkt 2:21 2.0+4.0i -- unboxed literal
TR opt: float-complex-i.rkt 2:3 1.0+2.0i -- unboxed literal
END
#<<END
-3.0+4.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ 1.0+2.0i (* +1.0i 2.0+4.0i))
