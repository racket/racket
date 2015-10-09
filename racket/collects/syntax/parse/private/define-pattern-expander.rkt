#lang racket/base
(require
  (for-syntax racket/base "parse.rkt" "keywords.rkt" "pattern-expander.rkt"))
(provide define-pattern-expander)

(define-syntax (define-pattern-expander stx)
  (syntax-parse stx
    [(_ (pe x ...) ~! body)
     #:fail-unless (and (identifier? #'pe)
                        (regexp-match "^~" (symbol->string (syntax-e #'pe))))
                   (format "pattern expander name must begin with ~~, given: ~a"
                           (syntax->datum #'pe))
     #'(define-syntax pe
         (pattern-expander (syntax-parser [(_ x ...) #'body])))]
    [(_ pe proc)
     #:when (identifier? #'pe)
     #:fail-unless (regexp-match "^~" (symbol->string (syntax-e #'pe)))
                   (format "pattern expander name must begin with ~~, given: ~a"
                           (syntax->datum #'pe))
     #'(define-syntax pe (pattern-expander proc))]))