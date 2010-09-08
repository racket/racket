#;
(
inexact-complex.rkt line 18 col 3 - 1.0+2.0i - unboxed literal
inexact-complex.rkt line 18 col 12 - 2.0+4.0i - unboxed literal
inexact-complex.rkt line 18 col 1 - + - unboxed binary inexact complex
inexact-complex.rkt line 18 col 0 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed inexact complex
inexact-complex.rkt line 19 col 3 - 1.0+2.0i - unboxed literal
inexact-complex.rkt line 19 col 12 - 2.0+4.0i - unboxed literal
inexact-complex.rkt line 19 col 1 - - - unboxed binary inexact complex
inexact-complex.rkt line 19 col 0 - (#%app - (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed inexact complex
3.0+6.0i
-1.0-2.0i
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(+ 1.0+2.0i 2.0+4.0i)
(- 1.0+2.0i 2.0+4.0i)
