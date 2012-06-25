#lang racket/base

(require racket/contract/region racket/contract/base
         syntax/location
         (for-syntax racket/base
                     syntax/parse
                     (prefix-in tr: "../typecheck/renamer.rkt")))

(provide require/contract define-ignored)

(define-syntax (define-ignored stx)
  (syntax-case stx ()
    [(_ name expr)
     (syntax-case (local-expand/capture-lifts #'expr
                                              'expression
                                              null #;(list #'define-values))
       (begin define-values)
       [(begin (define-values (n) e) ... e*)
        #`(begin (define-values (n) e) ...
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



;Requires an identifier from an untyped module into a typed module
;nm is the import
;hidden is an id that will end up being the actual definition
;nm will be bound to a rename transformer so that it is not provided
;with all-defined-out
(define-syntax (require/contract stx)
  (define-syntax-class renameable
    (pattern nm:id
             #:with orig-nm #'nm
             #:with orig-nm-r ((make-syntax-introducer) #'nm))
    (pattern (orig-nm:id nm:id)
             #:with orig-nm-r ((make-syntax-introducer) #'nm)))

  (syntax-parse stx
    [(require/contract nm:renameable hidden:id cnt lib)
     #`(begin (require (only-in lib [nm.orig-nm nm.orig-nm-r]))
              (define-syntax nm.nm (make-rename-transformer
                (syntax-property (syntax-property (quote-syntax hidden)
                    'not-free-identifier=? #t)
                  'not-provide-all-defined #t)))
              (define-ignored hidden
                (contract cnt
                          (get-alternate nm.orig-nm-r)
                          '(interface for #,(syntax->datum #'nm.nm))
                          (current-contract-region)
                          (quote nm.nm)
                          (quote-srcloc nm.nm))))]))
