#;
(
inexact-complex-float.rkt line 32 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float.rkt line 32 col 12 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float.rkt line 32 col 16 - 3.0+6.0i - unboxed literal
inexact-complex-float.rkt line 32 col 1 - + - unboxed binary inexact complex
inexact-complex-float.rkt line 32 col 0 - (#%app + (quote 1.0+2.0i) (quote 2.0) (quote 3.0+6.0i)) - unboxed inexact complex
inexact-complex-float.rkt line 33 col 3 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float.rkt line 33 col 7 - 2.0+4.0i - unboxed literal
inexact-complex-float.rkt line 33 col 16 - 3.0+6.0i - unboxed literal
inexact-complex-float.rkt line 33 col 1 - - - unboxed binary inexact complex
inexact-complex-float.rkt line 33 col 0 - (#%app - (quote 1.0) (quote 2.0+4.0i) (quote 3.0+6.0i)) - unboxed inexact complex
inexact-complex-float.rkt line 34 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float.rkt line 34 col 12 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float.rkt line 34 col 16 - 3.0+6.0i - unboxed literal
inexact-complex-float.rkt line 34 col 1 - - - unboxed binary inexact complex
inexact-complex-float.rkt line 34 col 0 - (#%app - (quote 1.0+2.0i) (quote 2.0) (quote 3.0+6.0i)) - unboxed inexact complex
inexact-complex-float.rkt line 35 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float.rkt line 35 col 12 - 2.0+4.0i - unboxed literal
inexact-complex-float.rkt line 35 col 21 - (quote 3.0) - float-coerce-expr in complex ops
inexact-complex-float.rkt line 35 col 1 - - - unboxed binary inexact complex
inexact-complex-float.rkt line 35 col 0 - (#%app - (quote 1.0+2.0i) (quote 2.0+4.0i) (quote 3.0)) - unboxed inexact complex
6.0+8.0i
-4.0-10.0i
-4.0-4.0i
-4.0-2.0i
)

#lang typed/scheme
#:optimize

(+ 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0 2.0+4.0i 3.0+6.0i)
(- 1.0+2.0i 2.0 3.0+6.0i)
(- 1.0+2.0i 2.0+4.0i 3.0)
