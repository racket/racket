#lang racket/base

(require racket/stxparam
         (for-syntax racket/base))

(provide fail)

(define-syntax-parameter fail
  (lambda (stx)
    (raise-syntax-error
     #f "used out of context: not in match pattern" stx)))
