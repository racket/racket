#;
(
TR opt: float-complex-sin.rkt 16:10 (+ (sin (* t 6.28)) 0.0+0.0i) -- unboxed binary float complex
TR opt: float-complex-sin.rkt 16:10 (+ (sin (* t 6.28)) 0.0+0.0i) -- unboxed float complex
TR missed opt: float-complex-sin.rkt 16:13 (sin (* t 6.28)) -- unary, arg float-arg-expr, return type not Float
TR opt: float-complex-sin.rkt 16:13 (sin (* t 6.28)) -- float-arg-expr in complex ops
TR missed opt: float-complex-sin.rkt 16:18 (* t 6.28) -- binary, args all float-arg-expr, return type not Float -- caused by: 16:21 t
TR opt: float-complex-sin.rkt 16:30 0.0+0.0i -- unboxed literal
-0.0031853017931379904+0.0i
)

#lang typed/scheme
#:optimize

((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
