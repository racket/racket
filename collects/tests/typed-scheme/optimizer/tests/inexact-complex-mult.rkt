#;
(
inexact-complex-mult.rkt line 14 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-mult.rkt line 14 col 12 - 2.0+4.0i - unboxed literal
inexact-complex-mult.rkt line 14 col 21 - 3.0+6.0i - unboxed literal
inexact-complex-mult.rkt line 14 col 1 - * - unboxed binary inexact complex
inexact-complex-mult.rkt line 14 col 0 - (#%app * (quote 1.0+2.0i) (quote 2.0+4.0i) (quote 3.0+6.0i)) - unboxed inexact complex
-66.0-12.0i
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(* 1.0+2.0i 2.0+4.0i 3.0+6.0i)
