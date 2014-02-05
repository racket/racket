#;#;
#<<END
TR opt: unboxed-let-functions2.rkt 3:20 x -- unboxed var -> table
TR opt: unboxed-let-functions2.rkt 3:42 y -- unboxed var -> table
TR opt: unboxed-let-functions2.rkt 3:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions2.rkt 4:18 (+ x y) -- unboxed binary float complex
TR opt: unboxed-let-functions2.rkt 4:21 x -- leave var unboxed
TR opt: unboxed-let-functions2.rkt 4:23 y -- leave var unboxed
TR opt: unboxed-let-functions2.rkt 5:17 2.0+4.0i -- unboxed literal
TR opt: unboxed-let-functions2.rkt 5:2 (f (+ 1.0+2.0i 2.0+4.0i) 3.0+6.0i) -- call to fun with unboxed args
TR opt: unboxed-let-functions2.rkt 5:2 (f (+ 1.0+2.0i 2.0+4.0i) 3.0+6.0i) -- unboxed call site
TR opt: unboxed-let-functions2.rkt 5:5 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions2.rkt 5:8 1.0+2.0i -- unboxed literal
TR opt: unboxed-let-functions2.rkt 6:5 3.0+6.0i -- unboxed literal
END
#<<END
6.0+12.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

;; function with multiple complex args
(let ((f (lambda: ((x :   Float-Complex) (y : Float-Complex))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0+6.0i))
