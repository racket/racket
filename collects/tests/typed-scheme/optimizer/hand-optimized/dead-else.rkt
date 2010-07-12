#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(begin (number? 3)
       (unsafe-fl+ 2.0 3.0))
(begin #t
       (unsafe-fl+ 2.0 3.0))
