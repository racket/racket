#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; function with multiple complex args
(let ((f (lambda: ((x : Inexact-Complex) (y : Inexact-Complex))
                  (+ x y))))
  (f (+ 1.0+2.0i 2.0+4.0i)
     3.0+6.0i))
