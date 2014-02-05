#;#;
#<<END
TR missed opt: complex-exp.rkt 2:3 (exp 0) -- non-complex value in complex arithmetic
TR opt: complex-exp.rkt 2:0 (- (exp 0) 1.0+2.0i) -- unboxed binary float complex
TR opt: complex-exp.rkt 2:11 1.0+2.0i -- unboxed literal
TR opt: complex-exp.rkt 2:3 (exp 0) -- non float complex in complex ops
TR opt: complex-exp.rkt 2:8 0 -- non float complex in complex ops
END
#<<END
0.0-2.0i

END
#lang typed/racket
#reader tests/typed-racket/optimizer/reset-port

(- (exp 0) 1.0+2.0i)
