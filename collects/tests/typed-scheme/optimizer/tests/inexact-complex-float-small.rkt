#;
(
inexact-complex-float-small.rkt line 36 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float-small.rkt line 36 col 12 - (quote 3.0) - float-coerce-expr in complex ops
inexact-complex-float-small.rkt line 36 col 1 - + - unboxed binary inexact complex
inexact-complex-float-small.rkt line 36 col 0 - (#%app + (quote 1.0+2.0i) (quote 3.0)) - unboxed inexact complex
inexact-complex-float-small.rkt line 37 col 3 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-small.rkt line 37 col 7 - 2.0+4.0i - unboxed literal
inexact-complex-float-small.rkt line 37 col 1 - + - unboxed binary inexact complex
inexact-complex-float-small.rkt line 37 col 0 - (#%app + (quote 1.0) (quote 2.0+4.0i)) - unboxed inexact complex
inexact-complex-float-small.rkt line 38 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float-small.rkt line 38 col 12 - (quote 3.0) - float-coerce-expr in complex ops
inexact-complex-float-small.rkt line 38 col 1 - - - unboxed binary inexact complex
inexact-complex-float-small.rkt line 38 col 0 - (#%app - (quote 1.0+2.0i) (quote 3.0)) - unboxed inexact complex
inexact-complex-float-small.rkt line 39 col 3 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-small.rkt line 39 col 7 - 2.0+4.0i - unboxed literal
inexact-complex-float-small.rkt line 39 col 1 - - - unboxed binary inexact complex
inexact-complex-float-small.rkt line 39 col 0 - (#%app - (quote 1.0) (quote 2.0+4.0i)) - unboxed inexact complex
inexact-complex-float-small.rkt line 40 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float-small.rkt line 40 col 15 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-small.rkt line 40 col 19 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-small.rkt line 40 col 13 - + - binary float
inexact-complex-float-small.rkt line 40 col 12 - (#%app + (quote 1.0) (quote 2.0)) - float-coerce-expr in complex ops
inexact-complex-float-small.rkt line 40 col 1 - + - unboxed binary inexact complex
inexact-complex-float-small.rkt line 40 col 0 - (#%app + (quote 1.0+2.0i) (#%app + (quote 1.0) (quote 2.0))) - unboxed inexact complex
4.0+2.0i
3.0+4.0i
-2.0+2.0i
-1.0-4.0i
4.0+2.0i
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(+ 1.0+2.0i 3.0)
(+ 1.0 2.0+4.0i)
(- 1.0+2.0i 3.0)
(- 1.0 2.0+4.0i)
(+ 1.0+2.0i (+ 1.0 2.0))
