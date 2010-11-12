#;
(
unboxed-let-functions1.rkt line 28 col 45 - x - unbox float-complex
unboxed-let-functions1.rkt line 28 col 47 - 3.0+6.0i - unboxed literal
unboxed-let-functions1.rkt line 28 col 43 - + - unboxed binary float complex
unboxed-let-functions1.rkt line 28 col 42 - (#%app + x (quote 3.0+6.0i)) - unboxed float complex
unboxed-let-functions1.rkt line 28 col 20 - x - unboxed var -> table
unboxed-let-functions1.rkt line 28 col 7 - f - unboxed function -> table
unboxed-let-functions1.rkt line 28 col 7 - f - fun -> unboxed fun
unboxed-let-functions1.rkt line 28 col 45 - x - leave var unboxed
unboxed-let-functions1.rkt line 28 col 47 - 3.0+6.0i - unboxed literal
unboxed-let-functions1.rkt line 28 col 43 - + - unboxed binary float complex
unboxed-let-functions1.rkt line 28 col 42 - (#%app + x (quote 3.0+6.0i)) - unboxed float complex
unboxed-let-functions1.rkt line 29 col 8 - 1.0+2.0i - unboxed literal
unboxed-let-functions1.rkt line 29 col 17 - 2.0+4.0i - unboxed literal
unboxed-let-functions1.rkt line 29 col 6 - + - unboxed binary float complex
unboxed-let-functions1.rkt line 29 col 3 - f - unboxed call site
unboxed-let-functions1.rkt line 29 col 3 - f - call to fun with unboxed args
6.0+12.0i
)

#lang typed/scheme
#:optimize



;; simple case, function with single complex arg
(let ((f (lambda: ((x : Inexact-Complex)) (+ x 3.0+6.0i))))
  (f (+ 1.0+2.0i 2.0+4.0i)))
