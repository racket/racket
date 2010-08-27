#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; function with a mix of complex and non-complex args
(let ((f (lambda: ((x : Inexact-Complex) (y : Float))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0))
