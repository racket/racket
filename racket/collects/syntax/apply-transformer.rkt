#lang racket/base

(require (for-template racket/base)
         racket/syntax)

(provide local-apply-transformer)

(define ((make-quoting-transformer transformer-proc) stx)
  (syntax-case stx ()
    [(_ form)
     (let ([result (transformer-proc #'form)])
       (unless (syntax? result)
         (raise-arguments-error 'local-apply-transformer
                                "received value from syntax expander was not syntax"
                                "received" result))
       #`(quote #,result))]))

(define (local-apply-transformer transformer stx context [intdef-ctxs '()])
  (unless (or (set!-transformer? transformer)
              (and (procedure? transformer)
                   (procedure-arity-includes? transformer 1)))
    (raise-argument-error 'local-apply-transformer
                          "(or/c (-> syntax? syntax?) set!-transformer?)"
                          transformer))
  (unless (syntax? stx)
    (raise-argument-error 'local-apply-transformer "syntax?" stx))
  (unless (or (eq? context 'expression)
              (eq? context 'top-level)
              (eq? context 'module)
              (eq? context 'module-begin)
              (list? context))
    (raise-argument-error 'local-apply-transformer
                          "(or/c 'expression 'top-level 'module 'module-begin list?)"
                          context))
  (unless (and (list? intdef-ctxs)
               (andmap internal-definition-context? intdef-ctxs))
    (raise-argument-error 'local-apply-transformer
                          "(listof internal-definition-context?)"
                          intdef-ctxs))
  (unless (syntax-transforming?)
    (raise-arguments-error 'local-apply-transformer "not currently expanding"))
  (let* ([intdef-ctx (syntax-local-make-definition-context #f #f)]
         [transformer-proc (if (set!-transformer? transformer)
                               (set!-transformer-procedure transformer)
                               transformer)]
         [transformer-id (internal-definition-context-introduce
                          intdef-ctx
                          (generate-temporary 'local-apply-transformer))]
         [intdef-ctxs* (cons intdef-ctx intdef-ctxs)])
    (syntax-local-bind-syntaxes
     (list transformer-id)
     #`(quote #,(make-quoting-transformer transformer-proc))
     intdef-ctx)
    (syntax-case (local-expand #`(#,transformer-id #,stx) context '() intdef-ctxs*) (quote)
      [(quote form) #'form])))
