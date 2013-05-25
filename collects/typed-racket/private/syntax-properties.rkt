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

(define-properties
  (plambda-property typechecker:plambda)
  (ignore-property typechecker:ignore)
  (ignore-some-property typechecker:ignore-some))

