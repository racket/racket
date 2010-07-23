#lang typed/scheme #:optimize

(require racket/unsafe/ops)

(let ((x (unsafe-fl+ 1.0 2.0)))
  x)
