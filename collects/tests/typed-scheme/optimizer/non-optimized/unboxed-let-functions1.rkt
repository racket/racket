#lang typed/scheme

(require racket/unsafe/ops)

;; simple case, function with single complex arg
(let ((f (lambda: ((x : Inexact-Complex)) (+ x 3.0+6.0i))))
  (f (+ 1.0+2.0i 2.0+4.0i)))
