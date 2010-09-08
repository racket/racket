#;
(
unboxed-let-functions3.rkt line 30 col 21 - x - unbox inexact-complex
unboxed-let-functions3.rkt line 30 col 23 - y - float-coerce-expr in complex ops
unboxed-let-functions3.rkt line 30 col 19 - + - unboxed binary inexact complex
unboxed-let-functions3.rkt line 30 col 18 - (#%app + x y) - unboxed inexact complex
unboxed-let-functions3.rkt line 29 col 20 - x - unboxed var -> table
unboxed-let-functions3.rkt line 29 col 7 - f - unboxed function -> table
unboxed-let-functions3.rkt line 29 col 7 - f - fun -> unboxed fun
unboxed-let-functions3.rkt line 30 col 21 - x - leave var unboxed
unboxed-let-functions3.rkt line 30 col 23 - y - float-coerce-expr in complex ops
unboxed-let-functions3.rkt line 30 col 19 - + - unboxed binary inexact complex
unboxed-let-functions3.rkt line 30 col 18 - (#%app + x y) - unboxed inexact complex
unboxed-let-functions3.rkt line 29 col 0 - (let-values (((f) (lambda (x y) (#%app + x y)))) (#%app f (#%app + (quote 1.0+2.0i) (quote 2.0+4.0i)) (quote 3.0))) - unboxed let bindings
unboxed-let-functions3.rkt line 31 col 8 - 1.0+2.0i - unboxed literal
unboxed-let-functions3.rkt line 31 col 17 - 2.0+4.0i - unboxed literal
unboxed-let-functions3.rkt line 31 col 6 - + - unboxed binary inexact complex
unboxed-let-functions3.rkt line 31 col 3 - f - unboxed call site
unboxed-let-functions3.rkt line 31 col 3 - f - call to fun with unboxed args
6.0+6.0i
)

#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; function with a mix of complex and non-complex args
(let ((f (lambda: ((x : Inexact-Complex) (y : Float))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0))
