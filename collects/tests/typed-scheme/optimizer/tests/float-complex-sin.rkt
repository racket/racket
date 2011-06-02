#;
(
float-complex-sin.rkt 16:10 (#%app + (#%app sin (#%app * t (quote 6.28))) (quote 0.0+0.0i)) -- unboxed float complex
float-complex-sin.rkt 16:11 + -- unboxed binary float complex
float-complex-sin.rkt 16:13 (#%app sin (#%app * t (quote 6.28))) -- float-coerce-expr in complex ops
float-complex-sin.rkt 16:13 (#%app sin (#%app * t (quote 6.28))) -- unary, arg float-arg-expr, return type not Float
float-complex-sin.rkt 16:18 (#%app * t (quote 6.28)) -- binary, args all float-arg-expr, return type not Float -- caused by: 16:21 t
float-complex-sin.rkt 16:30 0.0+0.0i -- unboxed literal
-0.0031853017931379904+0.0i
)

#lang typed/scheme
#:optimize

((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
