#lang racket/base
(require
  syntax/parse
  (for-syntax racket/base syntax/parse racket/syntax))

(define-syntax (define-properties stx)
  (define-syntax-class clause
    (pattern (root:id sym:id)
      #:with name (format-id #'root "~a-property" #'root)
      #:with syntax-class-name (format-id #'root "~a^" #'root)
      #:with symbol (generate-temporary #'sym)))

  (syntax-parse stx
    ((_ :clause ...)
     #`(begin
         (begin
           ;; TODO: make this an uninterned symbol once the phasing issue of the unit
           ;; tests is fixed
           (define symbol 'sym)
           (provide name syntax-class-name)
           (define name
              (case-lambda
                ((stx) (syntax-property stx symbol))
                ((stx value) (syntax-property stx symbol value))))
            (define-syntax-class syntax-class-name
              #:attributes (value)
              (pattern e
               #:attr value (name #'e)
               #:when (attribute value)))) ...))))

;;TODO add contracts on the properties
;;TODO make better interface for properties with values of only #t

(define-properties
  (plambda typechecker:plambda)
  (ignore typechecker:ignore)
  (ignore-some typechecker:ignore-some)
  (contract-def/maker typechecker:contract-def/maker)
  (contract-def typechecker:contract-def)
  (flat-contract-def typechecker:flat-contract-def)
  (external-check typechecker:external-check)
  (with-type typechecker:with-type)
  (type-ascription type-ascription)
  (type-inst type-inst)
  (type-label type-label)
  (type-dotted type-dotted)
  (exn-handler typechecker:exn-handler)
  (exn-body typechecker:exn-body)
  (with-handlers typechecker:with-handlers)
  (struct-info struct-info)
  (opt-lambda opt-lambda)
  (kw-lambda kw-lambda)
  (tail-position typechecker:called-in-tail-position)
  )

