#;#;
#<<END
TR opt: float-complex-fixnum.rkt 2:0 (+ (modulo 2 1) 1.0+2.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-fixnum.rkt 2:16 1.0+2.0i -- unboxed literal
TR opt: float-complex-fixnum.rkt 2:25 3.0+6.0i -- unboxed literal
TR opt: float-complex-fixnum.rkt 2:3 (modulo 2 1) -- binary nonzero fixnum
TR opt: float-complex-fixnum.rkt 2:3 (modulo 2 1) -- non float complex in complex ops
END
#<<END
4.0+8.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

(+ (modulo 2 1) 1.0+2.0i 3.0+6.0i)
