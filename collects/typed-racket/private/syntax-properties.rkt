#lang racket/base
(require (for-syntax racket/base syntax/parse))

(define-syntax define-properties
  (syntax-parser
    ((_ (name:id sym:id) ...)
     (with-syntax (((symbol ...) (generate-temporaries #'(sym ...))))
       #`(begin
           (begin
             ;; TODO: make this an uninterned symbol once the phasing issue of the unit
             ;; tests is fixed
             (define symbol 'sym)
             (provide name)
             (define name
               (case-lambda
                 ((stx) (syntax-property stx symbol))
                 ((stx value) (syntax-property stx symbol value))))) ...)))))

;;TODO add contracts on the properties
;;TODO make better interface for properties with values of only #t

(define-properties
  (plambda-property typechecker:plambda)
  (ignore-property typechecker:ignore)
  (ignore-some-property typechecker:ignore-some)
  (contract-def/maker-property typechecker:contract-def/maker)
  (contract-def-property typechecker:contract-def)
  (flat-contract-def-property typechecker:flat-contract-def)
  (external-check-property typechecker:external-check)
  (with-type-property typechecker:with-type)
  (type-ascription-property type-ascription)
  (type-inst-property type-inst)
  (type-label-property type-label)
  (type-dotted-property type-dotted)
  (exn-handler-property typechecker:exn-handler)
  (exn-body-property typechecker:exn-body)
  (with-handlers-property typechecker:with-handlers)
  (struct-info-property struct-info)
  (opt-lambda-property opt-lambda)
  (kw-lambda-property kw-lambda)
  (tail-position-property typechecker:called-in-tail-position)
  )

