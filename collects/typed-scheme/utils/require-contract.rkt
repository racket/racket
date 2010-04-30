#lang scheme/base

(require scheme/contract
         unstable/location
         (for-syntax scheme/base
                     syntax/kerncase 
                     syntax/parse                     
                     "../utils/tc-utils.rkt"
                     (prefix-in tr: "../private/typed-renaming.rkt")))

(provide require/contract define-ignored)

(define-syntax (define-ignored stx)
  (syntax-case stx ()
    [(_ name expr)
     (syntax-case (local-expand/capture-lifts #'expr
                                              'expression 
                                              null #;(list #'define-values))
       (begin define-values)
       [(begin (define-values (n) e) e*)
        #`(begin (define-values (n) e)
                 (define name #,(syntax-property #'e*
                                                 'inferred-name
                                                 (syntax-e #'name))))]
       [(begin e)
        #`(define name #,(syntax-property #'e
                                          'inferred-name
                                          (syntax-e #'name)))])]))


(define-syntax (get-alternate stx)
  (syntax-case stx ()
    [(_ id)
     (tr:get-alternate #'id)]))

(define-syntax (require/contract stx)
  (define-syntax-class renameable
    (pattern nm:id
             #:with r ((make-syntax-introducer) #'nm)))
  (syntax-parse stx
    [(require/contract nm:renameable cnt lib)
     #`(begin (require (only-in lib [nm nm.r]))
              (define-ignored nm 
                (contract cnt 
                          (get-alternate nm.r)
                          '(interface for #,(syntax->datum #'nm))
                          (current-contract-region)
                          (quote nm)
                          (quote-srcloc nm))))]
    [(require/contract (orig-nm:renameable nm:id) cnt lib)
     #`(begin (require (only-in lib [orig-nm orig-nm.r]))
              (define-ignored nm 
                (contract cnt 
                          (get-alternate orig-nm.r) 
                          '#,(syntax->datum #'nm)
                          (current-contract-region)
                          (quote nm)
                          (quote-srcloc nm))))]))
