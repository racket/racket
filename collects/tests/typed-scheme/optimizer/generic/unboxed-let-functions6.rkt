#lang typed/scheme #:optimize

(require racket/unsafe/ops racket/flonum)

(let: loop : Inexact-Complex ((z : Inexact-Complex 0.0+0.0i)
                              (l : (Listof Integer) '(1 2 3)))
      (if (null? l)
          (+ z 0.0+1.0i)
          (loop (+ z (car l))
                (cdr l))))
