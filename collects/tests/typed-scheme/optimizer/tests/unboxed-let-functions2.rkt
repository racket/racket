#;
(
unboxed-let-functions2.rkt line 35 col 21 - x - unbox float-complex
unboxed-let-functions2.rkt line 35 col 23 - y - unbox float-complex
unboxed-let-functions2.rkt line 35 col 19 - + - unboxed binary float complex
unboxed-let-functions2.rkt line 35 col 18 - (#%app + x y) - unboxed float complex
unboxed-let-functions2.rkt line 34 col 20 - x - unboxed var -> table
unboxed-let-functions2.rkt line 35 col 21 - x - unbox float-complex
unboxed-let-functions2.rkt line 35 col 23 - y - unbox float-complex
unboxed-let-functions2.rkt line 35 col 19 - + - unboxed binary float complex
unboxed-let-functions2.rkt line 35 col 18 - (#%app + x y) - unboxed float complex
unboxed-let-functions2.rkt line 34 col 42 - y - unboxed var -> table
unboxed-let-functions2.rkt line 34 col 7 - f - unboxed function -> table
unboxed-let-functions2.rkt line 34 col 7 - f - fun -> unboxed fun
unboxed-let-functions2.rkt line 35 col 21 - x - leave var unboxed
unboxed-let-functions2.rkt line 35 col 23 - y - leave var unboxed
unboxed-let-functions2.rkt line 35 col 19 - + - unboxed binary float complex
unboxed-let-functions2.rkt line 35 col 18 - (#%app + x y) - unboxed float complex
unboxed-let-functions2.rkt line 36 col 8 - 1.0+2.0i - unboxed literal
unboxed-let-functions2.rkt line 36 col 17 - 2.0+4.0i - unboxed literal
unboxed-let-functions2.rkt line 36 col 6 - + - unboxed binary float complex
unboxed-let-functions2.rkt line 37 col 5 - 3.0+6.0i - unboxed literal
unboxed-let-functions2.rkt line 36 col 3 - f - unboxed call site
unboxed-let-functions2.rkt line 36 col 3 - f - call to fun with unboxed args
6.0+12.0i
)

#lang typed/scheme
#:optimize



;; function with multiple complex args
(let ((f (lambda: ((x : Inexact-Complex) (y : Inexact-Complex))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0+6.0i))
