#lang racket/base
(require (for-syntax racket/base
                     racket/require-transform)
         "base.rkt")
(provide contract-in)

(define-syntax contract-in
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ m stx-ids ...)
        (let ()
          (define module-name (gensym 'm))
          (define-values (ids ctcs)
            (for/fold ([ids '()]
                       [ctcs '()])
                      ([id-pr (in-list (syntax->list #'(stx-ids ...)))])
              (syntax-case id-pr ()
                [(x ctc)
                 (let ()
                   (unless (identifier? #'x)
                     (raise-syntax-error 'contract-id "expected an identifier" stx #'x))
                   (values (cons #'x ids)
                           (cons #'ctc ctcs)))]
                [_
                 (raise-syntax-error 'contract-id "expected an identifier and a contract" stx id-pr)])))
          (define gen-ids (generate-temporaries ids))

          (define-values (pos-blame neg-blame)
            (let ()
              (define ids (generate-temporaries '(pos-blame neg-blame)))
              (values (list-ref ids 0)
                      (list-ref ids 1))))
          (syntax-local-lift-require-top-level-expression
           #`(define #,pos-blame #,(format "~s" (syntax->datum #'m))))
          (syntax-local-lift-require-top-level-expression
           #`(define #,neg-blame (current-contract-region)))
          (for ([id (in-list ids)]
                [gen-id (in-list gen-ids)]
                [ctc (in-list ctcs)])
            (syntax-local-lift-require-top-level-expression
             #`(define #,id
                 (contract #,ctc #,gen-id
                           #,pos-blame #,neg-blame
                           '#,id
                           (srcloc '#,(syntax-source id)
                                   #,(syntax-line id)
                                   #,(syntax-column id)
                                   #,(syntax-position id)
                                   #,(syntax-span id))))))

          (values (for/list ([id (in-list ids)]
                             [gen-id (in-list gen-ids)])
                    (import gen-id
                            (syntax-e id)
                            #'m
                            0
                            0
                            0
                            stx))
                  (list (import-source #'m 0))))]))))
