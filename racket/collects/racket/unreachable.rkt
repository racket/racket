#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/unsafe/ops unsafe-assert-unreachable))
(provide with-assert-unreachable)

(define-syntax (with-assert-unreachable stx)
  (syntax-case stx ()
    [(_) (raise-syntax-error
          'with-assert-unreachable
          "expected at least one expression on the body")]
    [(_ body ...)
     #'(if (variable-reference-from-unsafe? (#%variable-reference))
           (unsafe-assert-unreachable)
           (let-values () body ...))]))
