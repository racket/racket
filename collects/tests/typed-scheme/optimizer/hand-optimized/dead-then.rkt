#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(display (begin (number? "eh")
                (unsafe-fl+ 4.0 5.0)))
(display (begin #f
                (unsafe-fl+ 4.0 5.0)))
