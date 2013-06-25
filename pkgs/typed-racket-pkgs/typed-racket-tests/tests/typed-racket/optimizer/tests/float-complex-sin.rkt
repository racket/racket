#;
(
TR missed opt: float-complex-sin.rkt 22:18 (* t 6.28) -- all args float-arg-expr, result not Float -- caused by: 22:21 t
TR info: float-complex-sin.rkt 22:18 (* t 6.28) -- exact real arith
TR missed opt: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- all args float-arg-expr, result not Float -- caused by: 22:18 (* t 6.28)
TR info: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- exact real arith
TR missed opt: float-complex-sin.rkt 22:18 (* t 6.28) -- all args float-arg-expr, result not Float -- caused by: 22:21 t
TR info: float-complex-sin.rkt 22:18 (* t 6.28) -- exact real arith
TR missed opt: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- all args float-arg-expr, result not Float -- caused by: 22:18 (* t 6.28)
TR info: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- exact real arith
TR opt: float-complex-sin.rkt 22:13 (sin (* t 6.28)) -- float-arg-expr in complex ops
TR opt: float-complex-sin.rkt 22:30 0.0+0.0i -- unboxed literal
TR opt: float-complex-sin.rkt 22:10 (+ (sin (* t 6.28)) 0.0+0.0i) -- unboxed binary float complex
-0.0031853017931379904+0.0i
)



#lang typed/scheme
#:optimize
((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
