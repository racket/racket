#;#;
#<<END
TR opt: unboxed-let-functions4.rkt 26:32 x -- unboxed var -> table
TR opt: unboxed-let-functions4.rkt 26:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions4.rkt 27:18 (+ x y) -- unboxed binary float complex
TR opt: unboxed-let-functions4.rkt 27:18 (+ x y) -- unboxed binary float complex
TR opt: unboxed-let-functions4.rkt 27:21 x -- leave var unboxed
TR opt: unboxed-let-functions4.rkt 27:21 x -- unbox float-complex
TR opt: unboxed-let-functions4.rkt 27:23 y -- float-arg-expr in complex ops
TR opt: unboxed-let-functions4.rkt 27:23 y -- float-arg-expr in complex ops
TR opt: unboxed-let-functions4.rkt 28:2 (f 3.0 (+ 1.0+2.0i 2.0+4.0i)) -- call to fun with unboxed args
TR opt: unboxed-let-functions4.rkt 28:2 (f 3.0 (+ 1.0+2.0i 2.0+4.0i)) -- unboxed call site
TR opt: unboxed-let-functions4.rkt 29:17 2.0+4.0i -- unboxed literal
TR opt: unboxed-let-functions4.rkt 29:5 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions4.rkt 29:8 1.0+2.0i -- unboxed literal
END
#<<END
6.0+6.0i

END

#lang typed/scheme
#:optimize

;; function with a mix of complex and non-complex args, non-complex first
(let ((f (lambda: ((y : Float) (x : Float-Complex))
                  (+ x y))))
  (f 3.0
     (+ 1.0+2.0i 2.0+4.0i)))
