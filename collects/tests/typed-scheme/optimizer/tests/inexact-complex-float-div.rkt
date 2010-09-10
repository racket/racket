#;
(
inexact-complex-float-div.rkt line 47 col 62 - x - unbox inexact-complex
inexact-complex-float-div.rkt line 47 col 52 - real-part - unboxed inexact complex
inexact-complex-float-div.rkt line 48 col 62 - x - unbox inexact-complex
inexact-complex-float-div.rkt line 48 col 52 - imag-part - unboxed inexact complex
inexact-complex-float-div.rkt line 50 col 9 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 50 col 13 - 2.0+4.0i - unboxed literal
inexact-complex-float-div.rkt line 50 col 7 - / - unboxed binary inexact complex
inexact-complex-float-div.rkt line 50 col 6 - (#%app / (quote 1.0) (quote 2.0+4.0i)) - unboxed inexact complex
inexact-complex-float-div.rkt line 51 col 9 - 1.0+2.0i - unboxed literal
inexact-complex-float-div.rkt line 51 col 18 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 51 col 7 - / - unboxed binary inexact complex
inexact-complex-float-div.rkt line 51 col 6 - (#%app / (quote 1.0+2.0i) (quote 2.0)) - unboxed inexact complex
inexact-complex-float-div.rkt line 52 col 9 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 52 col 13 - 2.0+4.0i - unboxed literal
inexact-complex-float-div.rkt line 52 col 22 - 3.0+6.0i - unboxed literal
inexact-complex-float-div.rkt line 52 col 7 - / - unboxed binary inexact complex
inexact-complex-float-div.rkt line 52 col 6 - (#%app / (quote 1.0) (quote 2.0+4.0i) (quote 3.0+6.0i)) - unboxed inexact complex
inexact-complex-float-div.rkt line 53 col 9 - 1.0+2.0i - unboxed literal
inexact-complex-float-div.rkt line 53 col 18 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 53 col 22 - 3.0+6.0i - unboxed literal
inexact-complex-float-div.rkt line 53 col 7 - / - unboxed binary inexact complex
inexact-complex-float-div.rkt line 53 col 6 - (#%app / (quote 1.0+2.0i) (quote 2.0) (quote 3.0+6.0i)) - unboxed inexact complex
inexact-complex-float-div.rkt line 54 col 9 - 1.0+2.0i - unboxed literal
inexact-complex-float-div.rkt line 54 col 18 - 2.0+4.0i - unboxed literal
inexact-complex-float-div.rkt line 54 col 27 - (quote 3.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 54 col 7 - / - unboxed binary inexact complex
inexact-complex-float-div.rkt line 54 col 6 - (#%app / (quote 1.0+2.0i) (quote 2.0+4.0i) (quote 3.0)) - unboxed inexact complex
inexact-complex-float-div.rkt line 55 col 9 - 1.0+2.0i - unboxed literal
inexact-complex-float-div.rkt line 55 col 18 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 55 col 22 - (quote 3.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 55 col 7 - / - unboxed binary inexact complex
inexact-complex-float-div.rkt line 55 col 6 - (#%app / (quote 1.0+2.0i) (quote 2.0) (quote 3.0)) - unboxed inexact complex
inexact-complex-float-div.rkt line 56 col 9 - (quote 1.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 56 col 13 - (quote 2.0) - float-coerce-expr in complex ops
inexact-complex-float-div.rkt line 56 col 17 - 3.0+6.0i - unboxed literal
inexact-complex-float-div.rkt line 56 col 7 - / - unboxed binary inexact complex
inexact-complex-float-div.rkt line 56 col 6 - (#%app / (quote 1.0) (quote 2.0) (quote 3.0+6.0i)) - unboxed inexact complex
'("0.1000000000-0.2000000000" "0.50000000001.0000000000" "-0.0200000000-0.0266666667" "0.16666666670.0000000000" "0.16666666670.0000000000" "0.16666666670.3333333333" "0.0333333333-0.0666666667")
)

#lang typed/scheme
#:optimize

(map (lambda: ((x : Inexact-Complex))
              (string-append (real->decimal-string (real-part x) 10)
                             (real->decimal-string (imag-part x) 10)))
     (list
      (/ 1.0 2.0+4.0i)
      (/ 1.0+2.0i 2.0)
      (/ 1.0 2.0+4.0i 3.0+6.0i)
      (/ 1.0+2.0i 2.0 3.0+6.0i)
      (/ 1.0+2.0i 2.0+4.0i 3.0)
      (/ 1.0+2.0i 2.0 3.0)
      (/ 1.0 2.0 3.0+6.0i)))
