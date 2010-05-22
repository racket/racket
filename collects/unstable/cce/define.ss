#lang scheme

(require "private/define-core.ss"
         (for-syntax scheme/match
                     syntax/kerncase
                     "syntax.ss"))

(provide

 in-phase1 in-phase1/pass2

 block
 at-end

 declare-names
 define-renamings
 define-single-definition
 define-with-parameter

 define-if-unbound
 define-values-if-unbound
 define-syntax-if-unbound
 define-syntaxes-if-unbound)

(define-syntax (at-end stx)
  (syntax-case stx ()
    [(_ e ...)
     (match (syntax-local-context)
       ['module
         (begin
           (syntax-local-lift-module-end-declaration
            (syntax/loc stx (begin e ...)))
           (syntax/loc stx (begin)))]
       [ctx (syntax-error stx
                          "can only be used in module context; got: ~s"
                          ctx)])]))

(define-syntax-rule (define-with-parameter name parameter)
  (define-syntax-rule (name value body (... ...))
    (parameterize ([parameter value]) body (... ...))))

(define-syntax (#%definition stx0)
  (syntax-case stx0 ()
    [(_ form)
     (let* ([stx (head-expand #'form)])
       (syntax-case stx ( module
                          #%require
                          #%provide
                          define-values
                          define-syntaxes
                          define-values-for-syntax
                          begin )
         [(module . _) stx]
         [(#%require . _) stx]
         [(#%provide . _) stx]
         [(define-values . _) stx]
         [(define-syntaxes . _) stx]
         [(define-values-for-syntax . _) stx]
         [(begin d ...) (syntax/loc stx0 (begin (#%definition d) ...))]
         [_ (raise-syntax-error '#%definition "not a definition" stx0 stx)]))]))

(define-syntax (#%as-definition stx0)
  (syntax-case stx0 ()
    [(_ form)
     (let* ([stx (head-expand #'form)])
       (syntax-case stx ( module
                          #%require
                          #%provide
                          define-values
                          define-syntaxes
                          define-values-for-syntax
                          begin )
         [(module . _) stx]
         [(#%require . _) stx]
         [(#%provide . _) stx]
         [(define-values . _) stx]
         [(define-syntaxes . _) stx]
         [(define-values-for-syntax . _) stx]
         [(begin d ...) (syntax/loc stx0 (begin (#%as-definition d) ...))]
         [e
          (syntax/loc stx0
            (define-values [] (begin e (#%plain-app values))))]))]))

(define-syntax (#%as-expression stx0)
  (syntax-case stx0 ()
    [(_ form)
     (let* ([stx (head-expand #'form)]
            ;; pre-compute this to save duplicated code below
            [done (quasisyntax/loc stx0 (begin #,stx (#%plain-app void)))])
       (syntax-case stx ( module
                          #%require
                          #%provide
                          define-values
                          define-syntaxes
                          define-values-for-syntax
                          begin )
         [(module . _) done]
         [(#%require . _) done]
         [(#%provide . _) done]
         [(define-values . _) done]
         [(define-syntaxes . _) done]
         [(define-values-for-syntax . _) done]
         [(begin) (syntax/loc stx0 (#%plain-app void))]
         [(begin d ... e)
          (syntax/loc stx0 (begin (#%as-definition d) ... (#%as-expression e)))]
         [_ stx]))]))

(define-syntax-rule (block form ...)
  (let-values () (#%as-expression (begin form ...))))

(define-syntax (declare-names stx)
  (match (syntax-local-context)
    ['top-level
     (syntax-case stx []
       [(_ name ...) (syntax/loc stx (define-syntaxes [name ...] (values)))])]
    [_ (syntax/loc stx (begin))]))

(define-syntax-rule (define-renamings [new old] ...)
  (define-syntaxes [new ...] (values (make-rename-transformer #'old) ...)))

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
       ['module-begin (syntax-error stx "cannot be used as module body")])]))

(define-syntax (in-phase1/pass2 stx)
  (syntax-case stx []
    [(_ e)
     (match (syntax-local-context)
       [(? pair?)
        (syntax/loc stx (define-values [] (begin (in-phase1 e) (values))))]
       [(or 'expression 'top-level 'module 'module-begin)
        (syntax/loc stx (#%expression (in-phase1 e)))])]))
