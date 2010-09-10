#;
(
inexact-complex-sin.rkt line 14 col 13 - (#%app sin (#%app * t (quote 6.28))) - float-coerce-expr in complex ops
inexact-complex-sin.rkt line 14 col 30 - 0.0+0.0i - unboxed literal
inexact-complex-sin.rkt line 14 col 11 - + - unboxed binary inexact complex
inexact-complex-sin.rkt line 14 col 10 - (#%app + (#%app sin (#%app * t (quote 6.28))) (quote 0.0+0.0i)) - unboxed inexact complex
-0.0031853017931379904+0.0i
)

#lang typed/scheme
#:optimize

((lambda: ((t : Integer))
          (+ (sin (* t 6.28)) 0.0+0.0i))
 1)
