#lang racket/base

(require (for-syntax racket/base
                     "transformer.rkt"
                     syntax/parse))

(provide define-honu-operator/syntax)
(define-syntax (define-honu-operator/syntax stx)
  (syntax-parse stx
    [(_ name precedence associativity function)
     #'(define-syntax name (make-honu-operator precedence associativity function))]))
