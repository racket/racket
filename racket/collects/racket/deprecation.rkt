#lang racket/base


(provide define-deprecated-alias)


(require (for-syntax racket/base
                     "deprecation/transformer.rkt"))


(define-syntax (define-deprecated-alias stx)
  (syntax-case stx ()
    [(_ id target-id)
     (let ()
       (unless (identifier? #'id)
         (raise-syntax-error #false "expected an alias identifier" stx #'id (list stx)))
       (unless (identifier? #'target-id)
         (raise-syntax-error #false "expected a target identifier" stx #'target-id (list stx)))
       (syntax-property
        #`(define-syntax id (make-and-check-deprecated-alias #'target-id #:context #'#,stx))
        'disappeared-use
        (list (syntax-local-introduce #'target-id))))]))


(define-for-syntax (make-and-check-deprecated-alias target-id #:context context-stx)
  (unless (identifier-binding target-id (syntax-local-phase-level) #true)
    (raise-syntax-error #false
                        "target identifier not bound"
                        context-stx
                        target-id
                        (list context-stx)
                        #:exn exn:fail:syntax:unbound))
  (deprecated-alias target-id))
