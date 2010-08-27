#lang typed/scheme
#:optimize

(require racket/unsafe/ops)

;; top level
(make-polar 1.0 1.0)

;; nested
(+ 1.0+2.0i (make-polar 2.0 4.0))
