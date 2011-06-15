#;
(
TR opt: unboxed-let-functions2.rkt 26:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions2.rkt 26:7 f -- unboxed function -> table
TR opt: unboxed-let-functions2.rkt 26:20 x -- unboxed var -> table
TR opt: unboxed-let-functions2.rkt 26:42 y -- unboxed var -> table
TR opt: unboxed-let-functions2.rkt 27:18 (+ x y) -- unboxed float complex
TR opt: unboxed-let-functions2.rkt 27:19 + -- unboxed binary float complex
TR opt: unboxed-let-functions2.rkt 27:21 x -- leave var unboxed
TR opt: unboxed-let-functions2.rkt 27:21 x -- unbox float-complex
TR opt: unboxed-let-functions2.rkt 27:23 y -- leave var unboxed
TR opt: unboxed-let-functions2.rkt 27:23 y -- unbox float-complex
TR opt: unboxed-let-functions2.rkt 28:3 f -- call to fun with unboxed args
TR opt: unboxed-let-functions2.rkt 28:3 f -- unboxed call site
TR opt: unboxed-let-functions2.rkt 28:6 + -- unboxed binary float complex
TR opt: unboxed-let-functions2.rkt 28:8 1.0+2.0i -- unboxed literal
TR opt: unboxed-let-functions2.rkt 28:17 2.0+4.0i -- unboxed literal
TR opt: unboxed-let-functions2.rkt 29:5 3.0+6.0i -- unboxed literal
6.0+12.0i
)

#lang typed/scheme
#:optimize

;; function with multiple complex args
(let ((f (lambda: ((x :   Float-Complex) (y : Float-Complex))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0+6.0i))
