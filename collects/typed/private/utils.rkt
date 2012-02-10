#lang typed/racket/base

(define-syntax-rule (dt nm t)
  (begin (define-type-alias nm t) (provide nm)))

(provide dt)
