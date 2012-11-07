#lang racket/load
(require tests/eli-tester)

(module ex plai
  (define-type Type
    [Variant (field number?)]))
  
(define-syntax (exports-of stx)
 (syntax-case stx ()
   [(_ module-name)
    (let ([exports (syntax-local-module-exports (syntax->datum #'module-name))])
      #`(quote #,(cdaddr exports)))]))

(test (sort (exports-of 'ex) string-ci<? #:key symbol->string)
      =>
      '(make-Variant set-Variant-field! Type Type? Variant Variant-field Variant?))
