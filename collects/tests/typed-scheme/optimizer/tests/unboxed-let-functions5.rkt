#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; invalid: f "escapes", according to our analysis
(letrec: ((f : (Inexact-Complex -> Inexact-Complex)
             (lambda: ((x : Inexact-Complex))
                      (let: ((y : (Inexact-Complex -> Inexact-Complex) f))
                            x))))
         (f (+ 1.0+2.0i 2.0+4.0i)))
