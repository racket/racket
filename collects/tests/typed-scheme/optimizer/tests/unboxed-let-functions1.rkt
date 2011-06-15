#;
(
TR opt: unboxed-let-functions1.rkt 23:7 f -- fun -> unboxed fun
TR opt: unboxed-let-functions1.rkt 23:7 f -- unboxed function -> table
TR opt: unboxed-let-functions1.rkt 23:20 x -- unboxed var -> table
TR opt: unboxed-let-functions1.rkt 23:42 (+ x 3.0+6.0i) -- unboxed float complex
TR opt: unboxed-let-functions1.rkt 23:43 + -- unboxed binary float complex
TR opt: unboxed-let-functions1.rkt 23:45 x -- leave var unboxed
TR opt: unboxed-let-functions1.rkt 23:45 x -- unbox float-complex
TR opt: unboxed-let-functions1.rkt 23:47 3.0+6.0i -- unboxed literal
TR opt: unboxed-let-functions1.rkt 24:3 f -- call to fun with unboxed args
TR opt: unboxed-let-functions1.rkt 24:3 f -- unboxed call site
TR opt: unboxed-let-functions1.rkt 24:6 + -- unboxed binary float complex
TR opt: unboxed-let-functions1.rkt 24:8 1.0+2.0i -- unboxed literal
TR opt: unboxed-let-functions1.rkt 24:17 2.0+4.0i -- unboxed literal
6.0+12.0i
)

#lang typed/scheme
#:optimize

;; simple case, function with single complex arg
(let ((f (lambda: ((x :   Float-Complex)) (+ x 3.0+6.0i))))
  (f (+ 1.0+2.0i 2.0+4.0i)))
