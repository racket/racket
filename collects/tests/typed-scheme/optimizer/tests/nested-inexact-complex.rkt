#;
(
nested-inexact-complex.rkt line 15 col 3 - 1.0+2.0i - unboxed literal
nested-inexact-complex.rkt line 15 col 15 - 2.0+4.0i - unboxed literal
nested-inexact-complex.rkt line 15 col 24 - 3.0+6.0i - unboxed literal
nested-inexact-complex.rkt line 15 col 13 - - - unboxed binary inexact complex
nested-inexact-complex.rkt line 15 col 1 - + - unboxed binary inexact complex
nested-inexact-complex.rkt line 15 col 0 - (#%app + (quote 1.0+2.0i) (#%app - (quote 2.0+4.0i) (quote 3.0+6.0i))) - unboxed inexact complex
0.0+0.0i
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops)
(+ 1.0+2.0i (- 2.0+4.0i 3.0+6.0i))
