#lang typed/scheme #:optimize

(require racket/unsafe/ops)

(let: loop1 : Inexact-Complex
      ((x : (Listof Inexact-Complex) '(1.0+2.0i 2.0+4.0i))
       (r : Inexact-Complex 0.0+0.0i))
      (if (null? x)
          r
          (let: loop2 : Inexact-Complex
                ((y : (Listof Inexact-Complex) '(3.0+6.0i 4.0+8.0i))
                 (s : Inexact-Complex 0.0+0.0i))
                (if (null? y)
                    (loop1 (cdr x) (+ r s))
                    (loop2 (cdr y) (+ s (car x) (car y)))))))
