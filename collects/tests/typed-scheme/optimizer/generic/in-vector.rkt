#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(for: ((i : Integer (vector 1 2 3)))
      (display i))
