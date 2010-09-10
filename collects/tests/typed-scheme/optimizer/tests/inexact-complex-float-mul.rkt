#;
(
inexact-complex-float-mul.rkt line 42 col 3 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-mul.rkt line 42 col 7 - 2.0+4.0i - unboxed literal
inexact-complex-float-mul.rkt line 42 col 1 - * - unboxed binary inexact complex
inexact-complex-float-mul.rkt line 42 col 0 - (#%app * (quote 1.0) (quote 2.0+4.0i)) - unboxed inexact complex
inexact-complex-float-mul.rkt line 43 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float-mul.rkt line 43 col 12 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-mul.rkt line 43 col 1 - * - unboxed binary inexact complex
inexact-complex-float-mul.rkt line 43 col 0 - (#%app * (quote 1.0+2.0i) (quote 2.0)) - unboxed inexact complex
inexact-complex-float-mul.rkt line 44 col 3 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-mul.rkt line 44 col 7 - 2.0+4.0i - unboxed literal
inexact-complex-float-mul.rkt line 44 col 16 - 3.0+6.0i - unboxed literal
inexact-complex-float-mul.rkt line 44 col 1 - * - unboxed binary inexact complex
inexact-complex-float-mul.rkt line 44 col 0 - (#%app * (quote 1.0) (quote 2.0+4.0i) (quote 3.0+6.0i)) - unboxed inexact complex
inexact-complex-float-mul.rkt line 45 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float-mul.rkt line 45 col 12 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-mul.rkt line 45 col 16 - 3.0+6.0i - unboxed literal
inexact-complex-float-mul.rkt line 45 col 1 - * - unboxed binary inexact complex
inexact-complex-float-mul.rkt line 45 col 0 - (#%app * (quote 1.0+2.0i) (quote 2.0) (quote 3.0+6.0i)) - unboxed inexact complex
inexact-complex-float-mul.rkt line 46 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float-mul.rkt line 46 col 12 - 2.0+4.0i - unboxed literal
inexact-complex-float-mul.rkt line 46 col 21 - (quote 3.0) - float-coerce-expr in complex ops
inexact-complex-float-mul.rkt line 46 col 1 - * - unboxed binary inexact complex
inexact-complex-float-mul.rkt line 46 col 0 - (#%app * (quote 1.0+2.0i) (quote 2.0+4.0i) (quote 3.0)) - unboxed inexact complex
inexact-complex-float-mul.rkt line 47 col 3 - 1.0+2.0i - unboxed literal
inexact-complex-float-mul.rkt line 47 col 12 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-mul.rkt line 47 col 16 - (quote 3.0) - float-coerce-expr in complex ops
inexact-complex-float-mul.rkt line 47 col 1 - * - unboxed binary inexact complex
inexact-complex-float-mul.rkt line 47 col 0 - (#%app * (quote 1.0+2.0i) (quote 2.0) (quote 3.0)) - unboxed inexact complex
2.0+4.0i
2.0+4.0i
-18.0+24.0i
-18.0+24.0i
-18.0+24.0i
6.0+12.0i
)

#lang typed/scheme
#:optimize

(* 1.0 2.0+4.0i)
(* 1.0+2.0i 2.0)
(* 1.0 2.0+4.0i 3.0+6.0i)
(* 1.0+2.0i 2.0 3.0+6.0i)
(* 1.0+2.0i 2.0+4.0i 3.0)
(* 1.0+2.0i 2.0 3.0)
