#;
(
unboxed-let-functions3.rkt 29:21 x -- unbox float-complex
unboxed-let-functions3.rkt 29:23 y -- float-coerce-expr in complex ops
unboxed-let-functions3.rkt 29:19 + -- unboxed binary float complex
unboxed-let-functions3.rkt 29:18 (#%app + x y) -- unboxed float complex
unboxed-let-functions3.rkt 28:20 x -- unboxed var -> table
unboxed-let-functions3.rkt 28:7 f -- unboxed function -> table
unboxed-let-functions3.rkt 28:7 f -- fun -> unboxed fun
unboxed-let-functions3.rkt 29:21 x -- leave var unboxed
unboxed-let-functions3.rkt 29:23 y -- float-coerce-expr in complex ops
unboxed-let-functions3.rkt 29:19 + -- unboxed binary float complex
unboxed-let-functions3.rkt 29:18 (#%app + x y) -- unboxed float complex
unboxed-let-functions3.rkt 30:8 1.0+2.0i -- unboxed literal
unboxed-let-functions3.rkt 30:17 2.0+4.0i -- unboxed literal
unboxed-let-functions3.rkt 30:6 + -- unboxed binary float complex
unboxed-let-functions3.rkt 30:3 f -- unboxed call site
unboxed-let-functions3.rkt 30:3 f -- call to fun with unboxed args
6.0+6.0i
)

#lang typed/scheme
#:optimize



;; function with a mix of complex and non-complex args
(let ((f (lambda: ((x : Float-Complex) (y : Float))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0))
