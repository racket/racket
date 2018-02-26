#lang racket/base
(require "../syntax/scope.rkt"
         "../syntax/taint.rkt"
         "../namespace/core.rkt"
         "../syntax/match.rkt"
         "../syntax/error.rkt"
         "../syntax/module-binding.rkt"
         "../namespace/namespace.rkt"
         "require+provide.rkt"
         "main.rkt"
         "parsed.rkt"
         "context.rkt"
         "require.rkt"
         "def-id.rkt"
         "bind-top.rkt"
         "log.rkt")

(add-core-form!
 'define-values
 (lambda (s ctx)
   (log-expand ctx 'prim-define-values)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (raise-syntax-error #f "not allowed in an expression position" s))
   (define disarmed-s (syntax-disarm s))
   (define-match m s '(define-values (id ...) rhs))
   (define-values (ids syms) (as-expand-time-top-level-bindings (m 'id) s ctx))
   (define exp-rhs (expand (m 'rhs) (as-named-context ctx ids)))
   (if (expand-context-to-parsed? ctx)
       (parsed-define-values s ids syms exp-rhs)
       (rebuild
        s
        `(,(m 'define-values) ,ids ,exp-rhs)))))

(add-core-form!
 'define-syntaxes
 (lambda (s ctx)
   (log-expand ctx 'prim-define-syntaxes)
   (log-expand ctx 'prepare-env)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (raise-syntax-error #f "not allowed in an expression position" s))
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(define-syntaxes (id ...) rhs))
   (define-values (ids syms) (as-expand-time-top-level-bindings (m 'id) s ctx))
   (define exp-rhs (expand-transformer (m 'rhs) (as-named-context ctx ids)))
   (if (expand-context-to-parsed? ctx)
       (parsed-define-syntaxes s ids syms exp-rhs)
       (rebuild
        s
        `(,(m 'define-syntaxes) ,ids ,exp-rhs)))))

(add-core-form!
 'begin-for-syntax
 (lambda (s ctx)
   (raise-syntax-error #f "not allowed in an expression position" s)))

(add-core-form!
 '#%require
 (lambda (s ctx)
   (log-expand ctx 'prim-require)
   (unless (eq? (expand-context-context ctx) 'top-level)
     (raise-syntax-error #f "allowed only in a module or the top level" s))
   (define disarmed-s (syntax-disarm s))
   (define-match m disarmed-s '(#%require req ...))
   (define sc (new-scope 'macro)) ; to hide bindings
   ;; Check the `#%require` form syntax and trigger compile-time
   ;; instanations
   (parse-and-perform-requires! (for/list ([req (in-list (m 'req))])
                                  (add-scope req sc))
                                s
                                #:visit? #f
                                (expand-context-namespace ctx)
                                (expand-context-phase ctx)
                                (make-requires+provides #f)
                                #:who 'require
                                ;; We don't need to check for conflicts:
                                #:initial-require? #t)
   ;; Nothing to expand
   (if (expand-context-to-parsed? ctx)
       (parsed-require s)
       s)))

(add-core-form!
 '#%provide
 (lambda (s ctx)
   (log-expand ctx 'prim-provide)
   (raise-syntax-error #f "not allowed outside of a module body" s)))
