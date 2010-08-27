#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

(for/fold: : Inexact-Complex ((sum : Inexact-Complex 0.0+0.0i))
           ((i : Inexact-Complex '(1.0+2.0i 2.0+4.0i)))
      (+ i sum))
