#lang racket/base
;; Allow evaluation at phase1
(require (for-syntax racket/base syntax/parse))
(provide phase1-eval)

(define-namespace-anchor anchor)
(define namespace (namespace-anchor->empty-namespace anchor))
(define-syntax phase1-eval
  (syntax-parser
    [(_ form:expr ...)
     #'(eval-syntax (quote-syntax (begin-for-syntax form ...)) namespace)]))
