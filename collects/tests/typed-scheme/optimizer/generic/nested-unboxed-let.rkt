#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(let ((x (+ 1.0+2.0i 2.0+3.0i)))
  (let ((x (+ x 2.0+3.0i)))
    (+ x 3.0+6.0i)))
