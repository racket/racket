#lang racket/base

(require (for-syntax racket/base
                     racket/list
                     racket/match
                     syntax/kerncase
                     unstable/syntax))

(provide

 in-phase1 in-phase1/pass2

 at-end

 declare-names
 define-renamings
 define-single-definition
 define-with-parameter

 define-if-unbound
 define-values-if-unbound
 define-syntax-if-unbound
 define-syntaxes-if-unbound)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Definition Generalization
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax-rule (define-single-definition define-one define-many)
  (define-syntax define-one
    (syntax-rules []
      [(_ (head . args) . body) (define-one head (lambda args . body))]
      [(_ name expr) (define-many [name] expr)])))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;;  Potentially Redundant Bindings
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define-syntax (define-many-if-unbound stx)
  (syntax-case stx []
    [(_ def [name ...] expr)
     (let* ([ids (syntax->list #'(name ...))])
       (for ([bad (in-list ids)] #:when (not (identifier? bad)))
         (wrong-syntax bad "expected an identifier"))
       (let*-values ([(bound unbound) (partition identifier-binding ids)])
         (cond
          [(null? bound) (syntax/loc stx (def [name ...] expr))]
          [(null? unbound) (syntax/loc stx (def [] (values)))]
          [else (wrong-syntax
                 stx
                 "conflicting definitions for ~s; none for ~s"
                 (map syntax-e bound)
                 (map syntax-e unbound))])))]))

(define-syntax-rule (define-values-if-unbound [name ...] expr)
  (define-many-if-unbound define-values [name ...] expr))

(define-single-definition define-if-unbound define-values-if-unbound)

(define-syntax-rule (define-syntaxes-if-unbound [name ...] expr)
  (define-many-if-unbound define-syntaxes [name ...] expr))

(define-single-definition define-syntax-if-unbound define-syntaxes-if-unbound)

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

(define-syntax-rule (define-with-parameter name parameter)
  (define-syntax-rule (name value body (... ...))
    (parameterize ([parameter value]) body (... ...))))

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
       ['module-begin (wrong-syntax stx "cannot be used as module body")])]))

(define-syntax (in-phase1/pass2 stx)
  (syntax-case stx []
    [(_ e)
     (match (syntax-local-context)
       [(? pair?)
        (syntax/loc stx (define-values [] (begin (in-phase1 e) (values))))]
       [(or 'expression 'top-level 'module 'module-begin)
        (syntax/loc stx (#%expression (in-phase1 e)))])]))
