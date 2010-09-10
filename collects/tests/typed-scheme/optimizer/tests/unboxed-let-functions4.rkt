#;
(
unboxed-let-functions4.rkt line 30 col 21 - x - unbox inexact-complex
unboxed-let-functions4.rkt line 30 col 23 - y - float-coerce-expr in complex ops
unboxed-let-functions4.rkt line 30 col 19 - + - unboxed binary inexact complex
unboxed-let-functions4.rkt line 30 col 18 - (#%app + x y) - unboxed inexact complex
unboxed-let-functions4.rkt line 29 col 32 - x - unboxed var -> table
unboxed-let-functions4.rkt line 29 col 7 - f - unboxed function -> table
unboxed-let-functions4.rkt line 29 col 7 - f - fun -> unboxed fun
unboxed-let-functions4.rkt line 30 col 21 - x - leave var unboxed
unboxed-let-functions4.rkt line 30 col 23 - y - float-coerce-expr in complex ops
unboxed-let-functions4.rkt line 30 col 19 - + - unboxed binary inexact complex
unboxed-let-functions4.rkt line 30 col 18 - (#%app + x y) - unboxed inexact complex
unboxed-let-functions4.rkt line 29 col 0 - (let-values (((f) (lambda (y x) (#%app + x y)))) (#%app f (quote 3.0) (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)))) - unboxed let bindings
unboxed-let-functions4.rkt line 32 col 8 - 1.0+2.0i - unboxed literal
unboxed-let-functions4.rkt line 32 col 17 - 2.0+4.0i - unboxed literal
unboxed-let-functions4.rkt line 32 col 6 - + - unboxed binary inexact complex
unboxed-let-functions4.rkt line 31 col 3 - f - unboxed call site
unboxed-let-functions4.rkt line 31 col 3 - f - call to fun with unboxed args
6.0+6.0i
)

#lang typed/scheme
#:optimize



;; function with a mix of complex and non-complex args, non-complex first
(let ((f (lambda: ((y : Float) (x : Inexact-Complex))
                  (+ x y))))
  (f 3.0
     (+ 1.0+2.0i 2.0+4.0i)))
