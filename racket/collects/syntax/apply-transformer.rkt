#lang racket/base

(require racket/syntax)

(provide local-apply-transformer)

(define (local-apply-transformer transformer stx context [intdef-ctxs #f])
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
  (unless (or (eq? #f intdef-ctxs)
              (internal-definition-context? intdef-ctxs)
              (and (list? intdef-ctxs)
                   (andmap internal-definition-context? intdef-ctxs)))
    (raise-argument-error 'local-apply-transformer
                          "(or/c internal-definition-context? #f (listof internal-definition-context?))"
                          intdef-ctxs))
  (unless (syntax-transforming?)
    (raise-arguments-error 'local-apply-transformer "not currently expanding"))

  ; syntax-local-apply-transformer only supports one intdef; for backwards
  ; compatibility if given more than one, add inside-edge scopes for each.
  (define scoped-stx
    (if (list? intdef-ctxs)
        (for/fold ([stx stx])
                  ([intdef-ctx intdef-ctxs])
          (internal-definition-context-introduce intdef-ctx stx 'add))
        stx))
  
  (let ([transformer-proc (if (set!-transformer? transformer)
                              (set!-transformer-procedure transformer)
                              transformer)])
    (syntax-local-apply-transformer transformer-proc
                                    #f
                                    context
                                    (if (internal-definition-context? intdef-ctxs)
                                        intdef-ctxs
                                        #f)
                                    scoped-stx)))
