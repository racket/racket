#lang typed/scheme #:optimize
(require racket/unsafe/ops)
(for: ((i : Char "123"))
      (display i))
