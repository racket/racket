#;#;
#<<END
TR opt: float-complex-fixnum.rkt 17:0 (+ (modulo 2 1) 1.0+2.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-fixnum.rkt 17:16 1.0+2.0i -- unboxed literal
TR opt: float-complex-fixnum.rkt 17:25 3.0+6.0i -- unboxed literal
TR opt: float-complex-fixnum.rkt 17:3 (modulo 2 1) -- binary nonzero fixnum
TR opt: float-complex-fixnum.rkt 17:3 (modulo 2 1) -- float-arg-expr in complex ops
END
#<<END
4.0+8.0i

END

#lang typed/scheme
#:optimize

(+ (modulo 2 1) 1.0+2.0i 3.0+6.0i)
