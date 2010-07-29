#lang typed/scheme #:optimize

(require racket/unsafe/ops)

(letrec: ((f : (Inexact-Complex -> Inexact-Complex) (lambda (x) (+ x 2.0+4.0i)))
          (g : (Inexact-Complex -> Inexact-Complex) f)) ; f escapes! can't unbox it's args
  (f 1.0+2.0i))
