#;
(
TR opt: nested-unboxed-let.rkt 22:0 (let ((x (+ 1.0+2.0i 2.0+3.0i))) (let ((x (+ x 2.0+3.0i))) (+ x 3.0+6.0i))) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 22:9 (+ 1.0+2.0i 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 22:12 1.0+2.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 22:21 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 23:2 (let ((x (+ x 2.0+3.0i))) (+ x 3.0+6.0i)) -- unboxed let bindings
TR opt: nested-unboxed-let.rkt 23:11 (+ x 2.0+3.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 23:14 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 23:14 x -- unbox float-complex
TR opt: nested-unboxed-let.rkt 23:16 2.0+3.0i -- unboxed literal
TR opt: nested-unboxed-let.rkt 24:4 (+ x 3.0+6.0i) -- unboxed binary float complex
TR opt: nested-unboxed-let.rkt 24:7 x -- leave var unboxed
TR opt: nested-unboxed-let.rkt 24:7 x -- unbox float-complex
TR opt: nested-unboxed-let.rkt 24:9 3.0+6.0i -- unboxed literal
8.0+14.0i
)

#lang typed/scheme
#:optimize

(let ((x (+ 1.0+2.0i 2.0+3.0i)))
  (let ((x (+ x 2.0+3.0i)))
    (+ x 3.0+6.0i)))
