#;#;
#<<END
TR opt: unboxed-let-functions1.rkt 3:20 x -- unboxed var -> table
TR opt: unboxed-let-functions1.rkt 3:42 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions1.rkt 3:45 x -- leave var unboxed
TR opt: unboxed-let-functions1.rkt 3:47 3.0+6.0i -- unboxed literal
TR opt: unboxed-let-functions1.rkt 3:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions1.rkt 4:17 2.0+4.0i -- unboxed literal
TR opt: unboxed-let-functions1.rkt 4:2 (f (+ 1.0+2.0i 2.0+4.0i)) -- call to fun with unboxed args
TR opt: unboxed-let-functions1.rkt 4:2 (f (+ 1.0+2.0i 2.0+4.0i)) -- unboxed call site
TR opt: unboxed-let-functions1.rkt 4:5 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions1.rkt 4:8 1.0+2.0i -- unboxed literal
END
#<<END
6.0+12.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port

;; simple case, function with single complex arg
(let ((f (lambda: ((x :   Float-Complex)) (+ x 3.0+6.0i))))
  (f (+ 1.0+2.0i 2.0+4.0i)))
