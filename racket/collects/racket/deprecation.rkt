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
       #'(define-syntax id (deprecated-alias #'target-id)))]))
