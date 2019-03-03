#lang racket/base
(require '#%linklet)

(provide linklet?
         
         compile-linklet
         recompile-linklet
         eval-linklet         
         instantiate-linklet
         
         linklet-import-variables
         linklet-export-variables

         linklet-directory?
         hash->linklet-directory
         linklet-directory->hash
         
         linklet-bundle?
         hash->linklet-bundle
         linklet-bundle->hash
         
         instance?
         make-instance
         instance-name
         instance-data
         instance-variable-names
         instance-variable-value
         instance-set-variable-value!
         instance-unset-variable!

         variable-reference->instance
         
         correlated?
         datum->correlated
         correlated->datum
         correlated-e
         correlated-property
         correlated-property-symbol-keys
         
         correlated-source
         correlated-line
         correlated-column
         correlated-position
         correlated-span)

;; The `#%kernel` primitive table is more primitive than the
;; `#%kernel` module:
(define kernel (primitive-table '#%kernel))
(define-syntax-rule (bounce id ...)
  (begin (define id (hash-ref kernel 'id)) ...))
(bounce syntax?
        syntax-e
        datum->syntax
        syntax->datum
        syntax-property
        syntax-property-symbol-keys
        
        syntax-source
        syntax-line
        syntax-column
        syntax-position
        syntax-span)

(define (correlated? e)
  (syntax? e))

(define (datum->correlated d [srcloc #f] [props #f])
  (datum->syntax #f d srcloc props))

(define (correlated-e e)
  (syntax-e e))

(define (correlated->datum e)
  (syntax->datum e))

(define correlated-property
  (case-lambda
    [(e k) (syntax-property e k)]
    [(e k v) (syntax-property e k v)]))

(define (correlated-property-symbol-keys e)
  (syntax-property-symbol-keys e))

(define (correlated-source s) (syntax-source s))
(define (correlated-line s) (syntax-line s))
(define (correlated-column s) (syntax-column s))
(define (correlated-position s) (syntax-position s))
(define (correlated-span s) (syntax-span s))
