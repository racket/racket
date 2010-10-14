#;
(
unboxed-let-functions5.rkt line 20 col 15 - 1.0+2.0i - unboxed literal
unboxed-let-functions5.rkt line 20 col 24 - 2.0+4.0i - unboxed literal
unboxed-let-functions5.rkt line 20 col 13 - + - unboxed binary float complex
unboxed-let-functions5.rkt line 20 col 12 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed float complex
3.0+6.0i
)

#lang typed/scheme
#:optimize



;; invalid: f "escapes", according to our analysis
(letrec: ((f : (Inexact-Complex -> Inexact-Complex)
             (lambda: ((x : Inexact-Complex))
                      (let: ((y : (Inexact-Complex -> Inexact-Complex) f))
                            x))))
         (f (+ 1.0+2.0i 2.0+4.0i)))
