#;
(
unboxed-let-functions5.rkt line 20 col 22 - (let-values (((y) f)) x) - unboxed let bindings
unboxed-let-functions5.rkt line 18 col 0 - (letrec-values (((f) (lambda (x) (let-values (((y) f)) x)))) (#%app f (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)))) - unboxed let bindings
unboxed-let-functions5.rkt line 22 col 15 - 1.0+2.0i - unboxed literal
unboxed-let-functions5.rkt line 22 col 24 - 2.0+4.0i - unboxed literal
unboxed-let-functions5.rkt line 22 col 13 - + - unboxed binary inexact complex
unboxed-let-functions5.rkt line 22 col 12 - (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) - unboxed inexact complex
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
