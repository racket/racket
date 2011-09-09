#;
(
TR opt: unboxed-make-rectangular.rkt 25:0 (let ((x (make-rectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 25:9 (make-rectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 26:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 26:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 26:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 26:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 27:0 (let ((x (unsafe-make-flrectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 27:9 (unsafe-make-flrectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 28:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 28:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 28:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 28:7 2.0+4.0i -- unboxed literal
3.0+6.0i
3.0+6.0i
)

#lang typed/scheme
#:optimize


(require racket/unsafe/ops)

(let ((x (make-rectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
(let ((x (unsafe-make-flrectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
