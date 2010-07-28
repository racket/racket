#lang typed/scheme #:optimize

(require racket/unsafe/ops racket/flonum)

(let: loop : Inexact-Complex ((z : Inexact-Complex 0.0+0.0i)
                              (l : (Listof Integer) '(1 2 3)))
  (if (null? l)
      z ; boxed use. z should be unboxed anyway
      (loop (+ z (car l))
            (cdr l))))
