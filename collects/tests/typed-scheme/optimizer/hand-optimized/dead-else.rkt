#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(display (begin (number? 3)
                (unsafe-fl+ 2.0 3.0)))
(display (begin #t
                (unsafe-fl+ 2.0 3.0)))
