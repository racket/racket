#lang scheme/base
(provide assert)    

(define (assert v)
  (unless v
    (error "Assertion failed - value was #f"))
  v)
