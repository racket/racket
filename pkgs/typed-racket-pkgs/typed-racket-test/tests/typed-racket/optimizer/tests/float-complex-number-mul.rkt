#;#;
#<<END
TR opt: float-complex-number-mul.rkt 17:0 (imag-part (* 0.0+1.0i (* 1 2))) -- complex accessor elimination
TR opt: float-complex-number-mul.rkt 17:11 (* 0.0+1.0i (* 1 2)) -- unboxed binary float complex
TR opt: float-complex-number-mul.rkt 17:14 0.0+1.0i -- unboxed literal
TR opt: float-complex-number-mul.rkt 17:23 (* 1 2) -- fixnum bounded expr
TR opt: float-complex-number-mul.rkt 17:23 (* 1 2) -- float-arg-expr in complex ops
TR opt: float-complex-number-mul.rkt 17:26 1 -- float-arg-expr in complex ops
TR opt: float-complex-number-mul.rkt 17:26 1 -- float-arg-expr in complex ops
TR opt: float-complex-number-mul.rkt 17:28 2 -- float-arg-expr in complex ops
END
#<<END
2.0

END
#lang typed/racket
(imag-part (* 0.0+1.0i (* 1 2)))
