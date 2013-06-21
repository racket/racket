#;#;
#<<END
TR opt: unboxed-let-functions1.rkt 26:20 x -- unboxed var -> table
TR opt: unboxed-let-functions1.rkt 26:42 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions1.rkt 26:42 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions1.rkt 26:45 x -- leave var unboxed
TR opt: unboxed-let-functions1.rkt 26:45 x -- unbox float-complex
TR opt: unboxed-let-functions1.rkt 26:47 3.0+6.0i -- unboxed literal
TR opt: unboxed-let-functions1.rkt 26:47 3.0+6.0i -- unboxed literal
TR opt: unboxed-let-functions1.rkt 26:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions1.rkt 27:17 2.0+4.0i -- unboxed literal
TR opt: unboxed-let-functions1.rkt 27:2 (f (+ 1.0+2.0i 2.0+4.0i)) -- call to fun with unboxed args
TR opt: unboxed-let-functions1.rkt 27:2 (f (+ 1.0+2.0i 2.0+4.0i)) -- unboxed call site
TR opt: unboxed-let-functions1.rkt 27:5 (+ 1.0+2.0i 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-let-functions1.rkt 27:8 1.0+2.0i -- unboxed literal
END
#<<END
6.0+12.0i

END

#lang typed/scheme
#:optimize

;; simple case, function with single complex arg
(let ((f (lambda: ((x :   Float-Complex)) (+ x 3.0+6.0i))))
  (f (+ 1.0+2.0i 2.0+4.0i)))
