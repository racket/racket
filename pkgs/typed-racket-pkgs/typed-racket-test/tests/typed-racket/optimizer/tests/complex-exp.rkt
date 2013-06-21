#;#;
#<<END
TR opt: complex-exp.rkt 15:0 (- (exp 0) 1.0+2.0i) -- unboxed binary float complex
TR opt: complex-exp.rkt 15:11 1.0+2.0i -- unboxed literal
TR opt: complex-exp.rkt 15:3 (exp 0) -- unboxed unary float complex
TR opt: complex-exp.rkt 15:8 0 -- float-arg-expr in complex ops
END
#<<END
0.0-2.0i

END

#lang typed/racket

(- (exp 0) 1.0+2.0i)
