#lang typed/scheme

(require racket/unsafe/ops)

;; function with a mix of complex and non-complex args, non-complex first
(let ((f (lambda: ((y : Float) (x : Inexact-Complex))
                  (+ x y))))
  (f 3.0
     (+ 1.0+2.0i 2.0+4.0i)))
