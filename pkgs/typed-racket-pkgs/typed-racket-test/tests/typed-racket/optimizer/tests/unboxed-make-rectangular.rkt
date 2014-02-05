#;#;
#<<END
TR opt: unboxed-make-rectangular.rkt 5:6 (x (make-rectangular 1.0 2.0)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 5:9 (make-rectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 6:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 6:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 6:7 2.0+4.0i -- unboxed literal
TR opt: unboxed-make-rectangular.rkt 7:6 (x (unsafe-make-flrectangular 1.0 2.0)) -- unboxed let bindings
TR opt: unboxed-make-rectangular.rkt 7:9 (unsafe-make-flrectangular 1.0 2.0) -- make-rectangular elimination
TR opt: unboxed-make-rectangular.rkt 8:2 (+ x 2.0+4.0i) -- unboxed binary float complex
TR opt: unboxed-make-rectangular.rkt 8:5 x -- leave var unboxed
TR opt: unboxed-make-rectangular.rkt 8:7 2.0+4.0i -- unboxed literal
END
#<<END
3.0+6.0i
3.0+6.0i

END
#lang typed/scheme
#:optimize
#reader tests/typed-racket/optimizer/reset-port


(require racket/unsafe/ops)

(let ((x (make-rectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
(let ((x (unsafe-make-flrectangular 1.0 2.0)))
  (+ x 2.0+4.0i))
