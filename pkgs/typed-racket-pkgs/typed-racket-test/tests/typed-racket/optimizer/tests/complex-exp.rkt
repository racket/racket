#;#;
#<<END
TR missed opt: complex-exp.rkt 16:3 (exp 0) -- non-complex value in complex arithmetic
TR opt: complex-exp.rkt 16:0 (- (exp 0) 1.0+2.0i) -- unboxed binary float complex
TR opt: complex-exp.rkt 16:11 1.0+2.0i -- unboxed literal
TR opt: complex-exp.rkt 16:3 (exp 0) -- float-arg-expr in complex ops
TR opt: complex-exp.rkt 16:8 0 -- float-arg-expr in complex ops
END
#<<END
0.0-2.0i

END

#lang typed/racket

(- (exp 0) 1.0+2.0i)
