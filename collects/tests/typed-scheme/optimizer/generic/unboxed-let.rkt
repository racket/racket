#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(let* ((t1 (+ 1.0+2.0i 2.0+4.0i))
       (t2 (- t1 3.0+6.0i))
       (t3 4.0+8.0i))
  (+ t2 t3))
