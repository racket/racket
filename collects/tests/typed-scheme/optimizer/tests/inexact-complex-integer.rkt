#;
(
inexact-complex-integer.rkt line 13 col 3 - (#%app expt (quote 2) (quote 100)) - float-coerce-expr in complex ops
inexact-complex-integer.rkt line 13 col 16 - 1.0+2.0i - unboxed literal
inexact-complex-integer.rkt line 13 col 1 - + - unboxed binary inexact complex
inexact-complex-integer.rkt line 13 col 0 - (#%app + (#%app expt (quote 2) (quote 100)) (quote 1.0+2.0i)) - unboxed inexact complex
1.2676506002282294e+30+2.0i
)

#lang typed/scheme
#:optimize
(require racket/unsafe/ops racket/flonum)
(+ (expt 2 100) 1.0+2.0i)
