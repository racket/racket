#lang racket/base


(provide define-deprecated-alias)


(require (for-syntax racket/base
                     "deprecation/transformer.rkt"))


(define-syntax (define-deprecated-alias stx)
  (syntax-case stx
    ()
    [(_ id target-id)
     (let ()
       (unless (identifier? #'id)
         (raise-syntax-error #f "expected an alias identifier" stx #'id))
       (unless (identifier? #'target-id)
         (raise-syntax-error #f "expected a target identifier" stx #'target-id))
       (unless (identifier-binding #'target-id (syntax-local-phase-level) #true)
         (raise-syntax-error #f "target identifier not bound" stx #'target-id
                             #:exn exn:fail:syntax:unbound))
       (syntax-property #'(define-syntax id (deprecated-alias #'target-id))
                        'disappeared-use
                        (list (syntax-local-introduce #'target-id))))]))
