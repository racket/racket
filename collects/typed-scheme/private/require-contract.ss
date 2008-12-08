#lang scheme/base

(require scheme/contract (for-syntax scheme/base syntax/kerncase))
(provide require/contract define-ignored)

(define-syntax (define-ignored stx)
  (syntax-case stx ()
    [(_ name expr)
     (syntax-case (local-expand/capture-lifts #'expr
                                              'expression 
                                              (list #'define-values))
       (begin define-values)
       [(begin (define-values (n) e) e*)
        #`(begin (define-values (n) e)
                 (define name #,(syntax-property #'e*
                                                 'inferred-name
                                                 (syntax-e #'name))))]
       [(begin (begin e))
        #`(define name #,(syntax-property #'e
                                          'inferred-name
                                          (syntax-e #'name)))])]))

(define-syntax (require/contract stx)
  (syntax-case stx ()
    [(require/contract nm cnt lib)
     (identifier? #'nm)
     #`(begin (require (only-in lib [nm tmp]))     
              (define-ignored nm (contract cnt tmp '#,(syntax->datum #'nm) 'never-happen (quote-syntax nm))))]
    [(require/contract (orig-nm nm) cnt lib)     
     #`(begin (require (only-in lib [orig-nm tmp]))
              (define-ignored nm (contract cnt tmp '#,(syntax->datum #'nm) 'never-happen (quote-syntax nm))))]))
