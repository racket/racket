#lang typed/scheme #:optimize

(require racket/unsafe/ops)

(: x (Boxof Integer))
(define x (box 1))
(unsafe-unbox* x)
(unsafe-set-box*! x 2)
(unsafe-unbox* x)
