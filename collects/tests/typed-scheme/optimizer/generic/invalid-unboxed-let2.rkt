#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; unboxing of let bindings does not currently work with multiple values
(let-values (((t1 t2) (values (+ 1.0+2.0i 2.0+4.0i) (+ 3.0+6.0i 4.0+8.0i))))
  (+ t1 t2))
