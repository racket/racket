#lang racket/base
(define two 2)
(provide two)

(module* one #f
  (require (submod "." ".." three))
  (define one 1)
  (provide one two three))

(module three racket/base
  (define three 3)
  (provide three))

