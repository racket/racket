#lang racket/base
(require
  syntax/parse
  (for-syntax racket/base syntax/parse racket/syntax))

(define-syntax define-matcher
  (syntax-parser
    [(_ name:id prop:id)
     #'(define-syntax-class name
         #:attributes (value)
         (pattern e
          #:attr value (prop #'e)
          #:when (attribute value)))]))

(define-syntax (define-properties stx)
  (define-syntax-class clause
    (pattern (name:id sym:id #:mark)
      #:with syntax-class-name (format-id #'name "~a^" #'name)
      #:with symbol (generate-temporary #'sym)
      #:with function
        #'(λ (stx) (syntax-property stx symbol #t)))
    (pattern (root:id sym:id)
      #:with name (format-id #'root "~a-property" #'root)
      #:with syntax-class-name (format-id #'root "~a^" #'root)
      #:with symbol (generate-temporary #'sym)
      #:with function
        #'(case-lambda
            ((stx) (syntax-property stx symbol))
            ((stx value) (syntax-property stx symbol value)))))

  (syntax-parse stx
    [(_ :clause ...)
     #`(begin
         (begin
            ;; TODO: make this an uninterned symbol once the phasing issue of the unit
            ;; tests is fixed
            (define symbol 'sym)
            (provide name syntax-class-name)
            (define name function)
            (define-syntax-class syntax-class-name
              #:attributes (value)
              (pattern e
               #:attr value (syntax-property #'e symbol)
               #:when (attribute value)))) ...)]))

;;TODO add contracts on the properties

(define-properties
  (plambda typechecker:plambda)
  (ignore typechecker:ignore #:mark)
  (ignore-some typechecker:ignore-some #:mark)
  (ignore-some-expr typechecker:ignore-some)
  (contract-def/maker typechecker:contract-def/maker)
  (contract-def typechecker:contract-def)
  (flat-contract-def typechecker:flat-contract-def)
  (external-check typechecker:external-check)
  (with-type typechecker:with-type #:mark)
  (type-ascription type-ascription)
  (type-inst type-inst)
  (type-label type-label)
  (type-dotted type-dotted)
  (exn-predicate typechecker:exn-predicate)
  (exn-handler typechecker:exn-handler)
  (exn-body typechecker:exn-body #:mark)
  (exn-handlers typechecker:exn-handlers #:mark)
  (struct-info struct-info)
  (opt-lambda opt-lambda)
  (kw-lambda kw-lambda)
  (tail-position typechecker:called-in-tail-position #:mark)
  (tr:class tr:class #:mark)
  (tr:class:top-level tr:class:top-level)
  (tr:class:super-new tr:class:super-new)
  (tr:class:type-annotation tr:class:type-annotation)
  (tr:class:super tr:class:super)
  (tr:class:local-table tr:class:local-table)
  (tr:class:method tr:class:method)
  )

