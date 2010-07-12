#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(for: ((i : Integer #"123"))
      (display i))
