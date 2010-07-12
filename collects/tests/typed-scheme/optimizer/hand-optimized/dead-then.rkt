#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(begin (number? "eh")
       (unsafe-fl+ 4.0 5.0))
(begin #f
       (unsafe-fl+ 4.0 5.0))
