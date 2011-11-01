#lang racket/base

(require (for-syntax racket/base
                     racket/syntax
                     syntax/parse
                     racket/provide-transform
                     syntax/define)
         racket/contract)

(provide define/latent-contract activate-contract-out)

(begin-for-syntax
  (struct value/latent-contract (value contract)
    #:property prop:procedure
    (λ (v/lc stx)
      (define value (value/latent-contract-value v/lc))
      (syntax-case stx ()
        [(_ . args)  (quasisyntax/loc stx (#,value . args))]
        [_           (quasisyntax/loc stx #,value)]))))

(define-syntax (define/latent-contract stx)
  (syntax-parse stx
    [(_ (head . args) contract:expr body:expr ...+)
     (define-values (name value)
       (normalize-definition (syntax/loc stx (define (head . args) body ...)) #'lambda #t #t))
     (syntax-protect
      (quasisyntax/loc stx
        (define/latent-contract #,name contract #,value)))]
    [(_ name:id contract:expr value:expr)
     (with-syntax ([value-name     (format-id #f "~a" #'name)]
                   [contract-name  (format-id #f "~a-contract" #'name)])
       (syntax-protect
        (syntax/loc stx
          (begin (define value-name value)
                 (define contract-name contract)
                 (define-syntax name
                   (value/latent-contract #'value-name #'contract-name))))))]))

(define-for-syntax (activate->contract-out stx id)
  (let* ([err  (λ () (raise-syntax-error 'activate-contract-out "no latent contract" id))]
         [v/lc  (syntax-local-value id err)])
    (when (not (value/latent-contract? v/lc)) (err))
    (with-syntax ([contract  (value/latent-contract-contract v/lc)])
      (quasisyntax/loc stx [#,id contract]))))

(define-syntax activate-contract-out/end
  (make-provide-pre-transformer
   (λ (stx metas)
     (syntax-case stx ()
       [(_ id ...)  (with-syntax ([(item ...)  (for/list ([id  (in-list (syntax->list #'(id ...)))])
                                                 (activate->contract-out stx id))])
                      (pre-expand-export
                       (syntax-protect
                        (syntax/loc stx (contract-out item ...)))
                       metas))]))))

(define-for-syntax (metas->abs-metas metas)
  (map (λ (meta) (and meta (+ meta (syntax-local-phase-level))))
       (if (null? metas) '(0) metas)))

(define-for-syntax (make-lifting-provide-pre-transformer target-id)
  (make-provide-pre-transformer
   (λ (stx metas)
     (syntax-case stx ()
       [(_ args ...)  (let ()
                        (for ([meta  (in-list (metas->abs-metas metas))])
                          (syntax-local-lift-module-end-declaration
                           (syntax-protect
                            (quasisyntax/loc stx
                              (provide (for-meta #,meta (#,target-id args ...)))))))
                        (syntax/loc stx (combine-out)))]))))

(define-syntax activate-contract-out
  (make-lifting-provide-pre-transformer #'activate-contract-out/end))
