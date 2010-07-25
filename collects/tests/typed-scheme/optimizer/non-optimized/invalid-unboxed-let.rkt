#lang typed/scheme

(require racket/unsafe/ops)

(let ((t1 (+ 1.0+2.0i 2.0+4.0i)) ; can be unboxed
      (t2 (+ 3.0+6.0i 4.0+8.0i)) ; can't be unboxed
      (t3 1.0+2.0i) ; can't be unboxed
      (t4 1))
  (display (+ t1 t1))
  (display t2)
  (display t3)
  (display t4))
