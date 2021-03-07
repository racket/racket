#lang racket/base
(require (for-syntax racket/base)
         (only-in racket/unsafe/ops unsafe-assert-unreachable))
(provide (rename-out [assert-unreachable-helper assert-unreachable])
         with-assert-unreachable)

(define (assert-unreachable)
  (raise (exn:fail:contract "assert-unreachable: unreachable code reached"
                            (current-continuation-marks))))

(define-syntax (assert-unreachable-helper stx)
  (syntax-case stx ()
    [(_)
     #'(if (variable-reference-from-unsafe? (#%variable-reference))
           (unsafe-assert-unreachable)
           (assert-unreachable))]
    [(_ . args)
     #'(assert-unreachable . args)]
    [_ #'assert-unreachable]))

(define-syntax (with-assert-unreachable stx)
  (syntax-case stx ()
    [(_) (raise-syntax-error
          'with-assert-unreachable
          "expected at least one expression on the body")]
    [(_ body ...)
     #'(if (variable-reference-from-unsafe? (#%variable-reference))
           (unsafe-assert-unreachable)
           (let-values () body ...))]))
