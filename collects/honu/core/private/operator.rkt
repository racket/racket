#lang racket/base

(require (for-syntax racket/base
                     "transformer.rkt"
                     syntax/parse))

(provide define-honu-operator/syntax)
(define-syntax (define-honu-operator/syntax stx)
  (syntax-parse stx
    [(_ name precedence associativity binary-function)
     #'(define-syntax name (make-honu-operator precedence associativity binary-function #f))]
    [(_ name precedence associativity binary-function unary-function)
     #'(define-syntax name (make-honu-operator precedence associativity binary-function unary-function))]))
