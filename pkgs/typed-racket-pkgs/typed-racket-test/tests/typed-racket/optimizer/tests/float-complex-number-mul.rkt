#;#;
#<<END
TR opt: float-complex-number-mul.rkt 1:0 (imag-part (* 0.0+1.0i (* 1 2))) -- complex accessor elimination
TR opt: float-complex-number-mul.rkt 1:11 (* 0.0+1.0i (* 1 2)) -- unboxed binary float complex
TR opt: float-complex-number-mul.rkt 1:14 0.0+1.0i -- unboxed literal
TR opt: float-complex-number-mul.rkt 1:23 (* 1 2) -- fixnum bounded expr
TR opt: float-complex-number-mul.rkt 1:23 (* 1 2) -- non float complex in complex ops
TR opt: float-complex-number-mul.rkt 1:26 1 -- non float complex in complex ops
TR opt: float-complex-number-mul.rkt 1:26 1 -- non float complex in complex ops
TR opt: float-complex-number-mul.rkt 1:28 2 -- non float complex in complex ops
END
#<<END
2.0

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port
(imag-part (* 0.0+1.0i (* 1 2)))
