#lang racket/base
(require "../syntax/error.rkt"
         "../syntax/scope.rkt"
         "context.rkt"
         "syntax-id-error.rkt")

(provide raise-syntax-implicit-error)

(define (raise-syntax-implicit-error s sym trigger-id ctx)
  (define phase (expand-context-phase ctx))
  (define what
    (case sym
      [(#%app) "function application"]
      [(#%datum) "literal data"]
      [(#%top)
       (if (expand-context-allow-unbound? ctx)
           "reference to a top-level identifier"
           "reference to an unbound identifier")]))
  (define unbound? (and trigger-id (not (resolve trigger-id phase))))
  (define unbound-form (and unbound?
                            (not (eq? (syntax-e s) (syntax-e trigger-id)))
                            s))
  (raise-syntax-error #f
                      (format (if unbound?
                                  "unbound identifier;\n also, no ~a syntax transformer is bound~a"
                                  (string-append what " is not allowed;\n no ~a syntax transformer is bound~a"))
                              sym
                              (case phase
                                [(0) ""]
                                [(1) " in the transformer phase"]
                                [else (format " at phase ~a" phase)]))
                      (and unbound?
                           (or unbound-form
                               trigger-id))
                      (if unbound?
                          (and unbound-form trigger-id)
                          s)
                      null
                      (if unbound? (syntax-debug-info-string trigger-id ctx) "")))
