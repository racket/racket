#lang scheme/base

(require scheme/contract (for-syntax scheme/base syntax/kerncase 
                                     "../utils/tc-utils.ss"
                                     (prefix-in tr: "../private/typed-renaming.ss")))

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


(define-syntax (get-alternate stx)
  (syntax-case stx ()
    [(_ id)
     (tr:get-alternate #'id)]))

(define-syntax (require/contract stx)
  (syntax-case stx ()
    [(require/contract nm cnt lib)
     (identifier? #'nm)
     (begin       
       #`(begin (require (only-in lib [nm tmp]))     
                (define-ignored nm 
                  (contract cnt 
                            (get-alternate tmp)
                            '(interface for #,(syntax->datum #'nm))
                            'never-happen
                            (quote-syntax nm)))))]
    [(require/contract (orig-nm nm) cnt lib)     
     (begin 
       #`(begin (require (only-in lib [orig-nm tmp]))
                (define-ignored nm 
                  (contract cnt 
                            (get-alternate tmp) 
                            '#,(syntax->datum #'nm)
                            'never-happen
                            (quote-syntax nm)))))]))
