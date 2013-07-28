#;
(
TR opt: unboxed-make-rectangular.rkt 30:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 30:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 30:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 29:9 (make-rectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 29:0 (let ((x (make-rectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 30:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 30:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 30:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 32:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 32:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 32:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 31:9 (unsafe-make-flrectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 31:0 (let ((x (unsafe-make-flrectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 32:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 32:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 32:2 (+ x 2.0+4.0i) -- unboxed binary float complex
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
