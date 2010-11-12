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

(test (exports-of 'ex)
      =>
      '(Type set-Variant-field! make-Variant Variant? Variant-field Variant Type?))