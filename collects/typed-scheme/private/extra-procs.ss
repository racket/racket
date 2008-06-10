#lang scheme/base
(provide assert map*)

(define (assert v)
  (unless v
    (error "Assertion failed - value was #f"))
  v)

(define map* map)


