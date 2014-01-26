#;#;
#<<END
TR opt: float-complex-fixnum.rkt 22:0 (+ (modulo 2 1) 1.0+2.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-fixnum.rkt 22:0 (+ (modulo 2 1) 1.0+2.0i 3.0+6.0i) -- unboxed binary float complex
TR opt: float-complex-fixnum.rkt 22:16 1.0+2.0i -- leave var unboxed
TR opt: float-complex-fixnum.rkt 22:16 1.0+2.0i -- unboxed let bindings
TR opt: float-complex-fixnum.rkt 22:16 1.0+2.0i -- unboxed literal
TR opt: float-complex-fixnum.rkt 22:25 3.0+6.0i -- leave var unboxed
TR opt: float-complex-fixnum.rkt 22:25 3.0+6.0i -- unboxed let bindings
TR opt: float-complex-fixnum.rkt 22:25 3.0+6.0i -- unboxed literal
TR opt: float-complex-fixnum.rkt 22:3 (modulo 2 1) -- binary nonzero fixnum
TR opt: float-complex-fixnum.rkt 22:3 (modulo 2 1) -- non float complex in complex ops
END
#<<END
4.0+8.0i

END

#lang typed/scheme
#:optimize

(+ (modulo 2 1) 1.0+2.0i 3.0+6.0i)
