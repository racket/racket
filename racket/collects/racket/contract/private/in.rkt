#lang racket/base
(require (for-syntax racket/base
                     racket/require-transform
                     (rename-in syntax/private/boundmap
                                ;; the private version of the library
                                ;; (the one without contracts)
                                ;; has these old, wrong names in it.
                                [make-module-identifier-mapping make-free-identifier-mapping]
                                [module-identifier-mapping-get free-identifier-mapping-get]
                                [module-identifier-mapping-put! free-identifier-mapping-put!]))
         "base.rkt"
         "module-boundary-ctc.rkt"
         "in-out.rkt")
(provide contract-in)

(define-syntax contract-in
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ m clauses ...)
        (let ()
          (define-values (code remappings)
            (generate-in/out-code 'contract-in
                                  stx
                                  (syntax->list #'(clauses ...))
                                  #f
                                  #:unprotected-submodule-name #f
                                  #:just-check-errors? 'contract-in
                                  #:provide? #f))

          ;; when just-check-errors? is 'contact-in, then the `remappings`
          ;; that we get is the names of the structs, so we can import
          ;; them and then look at their values in the second stage

          (syntax-local-lift-require-top-level-form
           #`(require (contract-in-step-two #,stx #,remappings m clauses ...)))
          (define orig-ids (map car remappings))
          (define export-id (map cdr remappings))

          (values (for/list ([gen-id (in-list orig-ids)]
                             [id (in-list export-id)])
                    (import gen-id
                            (syntax-e id)
                            #'m
                            0
                            0
                            0
                            stx))
                  (list (import-source #'m 0))))]))))

(define-syntax contract-in-step-two
  (make-require-transformer
   (λ (stx)
     (syntax-case stx ()
       [(_ original-stx struct-name-remappings-as-stx m clauses ...)
        (let ()
          (define pos-blame (car (generate-temporaries '(pos-blame))))
          (syntax-local-lift-require-top-level-form
           #`(define #,pos-blame #,(format "~s" (syntax->datum #'m))))

          (define struct-name-remappings (make-free-identifier-mapping))
          (for ([struct-name-remapping (in-list (syntax->list #'struct-name-remappings-as-stx))])
            (define pr (syntax-e struct-name-remapping))
            (define orig-id (car pr))
            (define export-id (cdr pr))
            (free-identifier-mapping-put! struct-name-remappings export-id orig-id))

          (define-values (code remappings)
            (generate-in/out-code 'contract-in
                                  #'original-stx
                                  (syntax->list #'(clauses ...))
                                  pos-blame
                                  #:unprotected-submodule-name #f
                                  #:just-check-errors? #f
                                  #:provide? #f
                                  #:struct-name-remappings struct-name-remappings))

          (syntax-local-lift-require-top-level-form code)
          (define orig-ids (map car remappings))
          (define export-id (map cdr remappings))

          (values (for/list ([gen-id (in-list orig-ids)]
                             [id (in-list export-id)])
                    (import gen-id
                            (syntax-e id)
                            #'m
                            0
                            0
                            0
                            #'original-stx))
                  (list (import-source #'m 0))))]))))
