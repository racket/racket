#lang typed/scheme

(require racket/unsafe/ops)

(: x (Boxof Integer))
(define x (box 1))
(unbox x)
(set-box! x 2)
(unbox x)
