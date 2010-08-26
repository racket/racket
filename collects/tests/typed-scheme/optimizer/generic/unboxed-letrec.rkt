#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(letrec: ((f : (Any -> Any) (lambda: ((x : Any)) (f x)))
          (x : Inexact-Complex 1.0+2.0i)
          (y : Inexact-Complex (+ 2.0+4.0i 3.0+6.0i)))
  (+ x y))
