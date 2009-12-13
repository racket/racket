#lang scheme/base
(provide assert)

(define (assert v [pred values])
  (unless (pred v)
    (error "Assertion failed"))
  v)

