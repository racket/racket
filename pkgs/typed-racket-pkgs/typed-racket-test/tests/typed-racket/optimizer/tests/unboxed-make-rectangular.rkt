#;#;
#<<END
TR opt: unboxed-make-rectangular.rkt 34:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 34:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 34:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 33:9 (make-rectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 33:0 (let ((x (make-rectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 34:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 34:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 34:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 36:5 x -- unbox float-complex
TR opt: unboxed-make-rectangular.rkt 36:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 36:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 35:9 (unsafe-make-flrectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 35:0 (let ((x (unsafe-make-flrectangular 1.0 2.0))) (+ x 2.0+4.0i)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 36:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 36:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 36:2 (+ x 2.0+4.0i) -- unboxed binary float complex

END
#<<END
3.0+6.0i
3.0+6.0i

END

#lang typed/scheme
#:optimize


(require racket/unsafe/ops)

(let ((x (make-rectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
(let ((x (unsafe-make-flrectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
