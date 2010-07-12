#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(if (number? 3)
    (+ 2.0 3.0)
    (+ 4.0 5.0))
(if #t
    (+ 2.0 3.0)
    (+ 4.0 5.0))
