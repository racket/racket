#lang racket/base
(require (for-syntax racket/base
                     racket/list
                     racket/match
                     racket/syntax
                     unstable/syntax))
(provide in-phase1
         in-phase1/pass2
         at-end)

(define-syntax (at-end stx)
  (syntax-case stx ()
    [(_ e ...)
     (match (syntax-local-context)
       ['module
         (begin
           (syntax-local-lift-module-end-declaration
            (syntax/loc stx (begin e ...)))
           (syntax/loc stx (begin)))]
       [ctx (wrong-syntax stx
                          "can only be used in module context; got: ~s"
                          ctx)])]))

(define-syntax (in-phase1 stx)
  (syntax-case stx []
    [(_ e)
     (match (syntax-local-context)
       ['expression (syntax/loc stx (let-syntax ([dummy e]) (void)))]
       [(or 'module 'top-level (? pair?))
        (syntax/loc stx
          (begin
            (define-syntax (macro stx*) (begin e (syntax/loc stx* (begin))))
            (macro)))]
       ['module-begin (wrong-syntax stx "cannot be used as module body")])]))

(define-syntax (in-phase1/pass2 stx)
  (syntax-case stx []
    [(_ e)
     (match (syntax-local-context)
       [(? pair?)
        (syntax/loc stx (define-values [] (begin (in-phase1 e) (values))))]
       [(or 'expression 'top-level 'module 'module-begin)
        (syntax/loc stx (#%expression (in-phase1 e)))])]))
