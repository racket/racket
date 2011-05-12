#;
(
float-complex-fixnum.rkt 17:4 modulo -- binary nonzero fixnum
float-complex-fixnum.rkt 17:4 modulo -- binary nonzero fixnum
float-complex-fixnum.rkt 17:4 modulo -- binary nonzero fixnum
float-complex-fixnum.rkt 17:3 (#%app modulo (quote 2) (quote 1)) -- float-coerce-expr in complex ops
float-complex-fixnum.rkt 17:16 1.0+2.0i -- unboxed literal
float-complex-fixnum.rkt 17:25 3.0+6.0i -- unboxed literal
float-complex-fixnum.rkt 17:1 + -- unboxed binary float complex
float-complex-fixnum.rkt 17:0 (#%app + (#%app modulo (quote 2) (quote 1)) (quote 1.0+2.0i) (quote 3.0+6.0i)) -- unboxed float complex
4.0+8.0i
)

#lang typed/scheme
#:optimize

(+ (modulo 2 1) 1.0+2.0i 3.0+6.0i)
