#lang racket/base
(require (for-syntax racket/base
                     racket/require-transform)
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
          (define pos-blame (car (generate-temporaries '(pos-blame))))
          (syntax-local-lift-require-top-level-form
           #`(define #,pos-blame #,(format "~s" (syntax->datum #'m))))

          (define-values (code remappings)
            (generate-in/out-code 'contract-in
                                  stx
                                  (syntax->list #'(clauses ...))
                                  #f ;; unprotected-submodule-name
                                  #f ;; just-check-errors?
                                  #f ;; provide?
                                  pos-blame))
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
                            stx))
                  (list (import-source #'m 0))))]))))
