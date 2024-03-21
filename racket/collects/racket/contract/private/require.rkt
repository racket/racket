#lang racket/base
(require (for-syntax racket/base
                     racket/require-transform)
         "base.rkt"
         "provide.rkt")
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

          (define pos-blame (car (generate-temporaries '(pos-blame))))
          (syntax-local-lift-require-top-level-expression
           #`(define #,pos-blame #,(format "~s" (syntax->datum #'m))))
          (for ([id (in-list ids)]
                [gen-id (in-list gen-ids)]
                [ctc (in-list ctcs)])
            (syntax-local-lift-require-top-level-expression
             #`(define-module-boundary-contract #,id
                 #,gen-id
                 #,ctc
                 #:pos-source #,pos-blame
                 #:name-for-blame #,id
                 #:srcloc
                 (srcloc '#,(syntax-source id)
                         #,(syntax-line id)
                         #,(syntax-column id)
                         #,(syntax-position id)
                         #,(syntax-span id))
                 #:lift-to-end? #f)))

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
