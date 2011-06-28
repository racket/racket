#;
(
TR opt: float-complex-sin.rkt 14:10 (+ (sin (* t 6.28)) 0.0+0.0i) -- unboxed binary float complex
TR missed opt: float-complex-sin.rkt 14:13 (sin (* t 6.28)) -- all args float-arg-expr, result not Float -- caused by: 14:21 t (2 times)
TR opt: float-complex-sin.rkt 14:13 (sin (* t 6.28)) -- float-arg-expr in complex ops
TR opt: float-complex-sin.rkt 14:30 0.0+0.0i -- unboxed literal
-0.0031853017931379904+0.0i
)

#lang typed/scheme
#:optimize

((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
