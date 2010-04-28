#lang racket/base
(require (for-syntax racket
                     web-server/lang/closure))

(provide define-closure)

(define-syntax (define-closure stx)
  (syntax-case stx ()
    [(_ tag formals (free-vars ...) body)
     (local
       [(define-values (make-CLOSURE-id CLOSURE?-id CLOSURE-env-id)
          (define-closure! #'tag #'(free-vars ...) (syntax/loc stx (lambda formals body))))
        (define make-tag
          (datum->syntax stx (string->symbol (format "make-~a" (syntax->datum #'tag))) stx))
        (define tag-env
          (datum->syntax stx (string->symbol (format "~a-env" (syntax->datum #'tag))) stx))
        (define tag?
          (datum->syntax stx (string->symbol (format "~a?" (syntax->datum #'tag))) stx))]
       (quasisyntax/loc stx
         (begin
           (define #,make-tag #,make-CLOSURE-id)
           (define #,tag? #,CLOSURE?-id)
           (define #,tag-env #,CLOSURE-env-id))))]))
