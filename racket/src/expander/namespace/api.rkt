#lang racket/base
(require (only-in "../syntax/syntax.rkt" syntax-mpi-shifts empty-syntax)
         (only-in "../syntax/scope.rkt" add-scopes push-scope syntax-scope-set)
         (only-in "../syntax/fallback.rkt" fallback-first)
         (only-in "../syntax/binding.rkt" resolve+shift syntax-transfer-shifts)
         "../syntax/module-binding.rkt"
         "../syntax/api.rkt"
         "../syntax/error.rkt"
         "../syntax/mapped-name.rkt"
         "namespace.rkt"
         "module.rkt"
         "attach.rkt"
         "core.rkt"
         "../common/set.rkt"
         "../common/phase.rkt"
         "../expand/require+provide.rkt"
         "../expand/context.rkt"
         "../expand/require.rkt"
         "../common/module-path.rkt"
         "../common/contract.rkt"
         "../expand/protect.rkt"
         "../expand/env.rkt"
         "../expand/binding-to-module.rkt"
         "../host/linklet.rkt")

(provide make-empty-namespace
         
         namespace-syntax-introduce
         namespace-module-identifier
         namespace-symbol->identifier
          
         namespace-require
         namespace-require/copy
         namespace-require/constant
         namespace-require/expansion-time
         
         namespace-variable-value
         namespace-set-variable-value!
         namespace-undefine-variable!
         
         namespace-mapped-symbols

         namespace-base-phase)

(define (make-empty-namespace)
  (define current-ns (current-namespace))
  (define phase (namespace-phase current-ns))
  (define ns (namespace->namespace-at-phase (make-namespace)
                                            phase))
  ;; For historical reasons, an empty namespace isn't actually
  ;; empty; we always carry '#%kernel along
  (namespace-attach-module current-ns ''#%kernel ns)
  (namespace-primitive-module-visit! ns '#%kernel)
  ns)

(define (namespace-syntax-introduce s [ns (current-namespace)])
  (check 'namespace-syntax-introduce syntax? s)
  (check 'namespace-syntax-introduce namespace? ns)
  (define root-ctx (namespace-get-root-expand-ctx ns))
  (define post-scope (root-expand-context-post-expansion-scope root-ctx))
  (define other-namespace-scopes (for/list ([sc (in-set
                                                 ;; `all-scopes-stx` corresponds to the initial import
                                                 (syntax-scope-set (root-expand-context-all-scopes-stx root-ctx)
                                                                   (namespace-phase ns)))]
                                            #:unless (equal? sc post-scope))
                                   sc))
  (define (add-ns-scopes s)
    (syntax-transfer-shifts (add-scopes (push-scope s post-scope)
                                        other-namespace-scopes)
                            (root-expand-context-all-scopes-stx root-ctx)
                            (or (namespace-declaration-inspector ns)
                                (current-code-inspector))
                            #:non-source? #t))
  (define maybe-module-id
    (and (pair? (syntax-e s))
         (identifier? (car (syntax-e s)))
         (add-ns-scopes (car (syntax-e s)))))
  (cond
   [(and maybe-module-id
         (free-identifier=? maybe-module-id
                            (namespace-module-identifier ns)
                            (namespace-phase ns)))
    ;; The given syntax object starts `module`, so only add scope to `module`:
    (datum->syntax s (cons maybe-module-id (cdr (syntax-e s))) s s)]
   [else
    ;; Add scope everywhere:
    (add-ns-scopes s)]))

(define (namespace-module-identifier [where (current-namespace)])
  (unless (or (namespace? where)
              (phase? where))
    (raise-argument-error 'namespace-module-identifier
                          (string-append "(or/c namespace? " phase?-string ")")
                          where))
  (datum->syntax (syntax-shift-phase-level core-stx
                                           (if (namespace? where)
                                               (namespace-phase where)
                                               where))
                 'module))

(define (namespace-symbol->identifier sym)
  (check 'namespace-symbol->identifier symbol? sym)
  (namespace-syntax-introduce (datum->syntax #f sym)))

;; ----------------------------------------

(define (do-namespace-require #:run? [run? #t] #:visit? [visit? #f]
                              who req ns
                              #:copy-variable-phase-level [copy-variable-phase-level #f]
                              #:copy-variable-as-constant? [copy-variable-as-constant? #f]
                              #:skip-variable-phase-level [skip-variable-phase-level #f])
  (check who namespace? ns)
  (define ctx-stx (add-scopes empty-syntax
                              (root-expand-context-module-scopes
                               (namespace-get-root-expand-ctx ns))))
  (cond
   [(or (module-path-index? req)
        (module-path? req))
    (perform-require! (if (module-path-index? req)
                          req
                          (module-path-index-join req #f))
                      #f #f
                      ctx-stx ns
                      #:run? run?
                      #:visit? visit?
                      #:phase-shift (namespace-phase ns)
                      #:run-phase (namespace-phase ns)
                      #:copy-variable-phase-level copy-variable-phase-level
                      #:copy-variable-as-constant? copy-variable-as-constant?
                      #:skip-variable-phase-level skip-variable-phase-level
                      #:who who)]
   [else
    ;; Slow way -- to allow renaming, check for conflicts, etc.
    (parse-and-perform-requires! #:run? run?
                                 #:visit? visit?
                                 (list (datum->syntax ctx-stx req))
                                 #f
                                 ns
                                 (namespace-phase ns)
                                 (make-requires+provides #f)
                                 #:skip-variable-phase-level skip-variable-phase-level
                                 #:who who)]))

(define (namespace-require req [ns (current-namespace)])
  (do-namespace-require 'namespace-require req ns))

(define (namespace-require/expansion-time req [ns (current-namespace)])
  (do-namespace-require #:run? #f #:visit? #t 'namespace-require/expansion-time req ns))
  
(define (namespace-require/constant req [ns (current-namespace)])
  (do-namespace-require 'namespace-require/constant req ns
                        #:copy-variable-phase-level 0
                        #:copy-variable-as-constant? #t))

(define (namespace-require/copy req [ns (current-namespace)])
  (do-namespace-require 'namespace-require/copy req ns
                        #:copy-variable-phase-level 0
                        #:skip-variable-phase-level 0))

;; ----------------------------------------

(define (namespace-variable-value sym
                                  [use-mapping? #f]
                                  [failure-thunk #f]
                                  [ns (current-namespace)])
  (check 'namespace-variable-value symbol? sym)
  (unless (or (not failure-thunk)
              (and (procedure? failure-thunk)
                   (procedure-arity-includes? failure-thunk 0)))
    (raise-argument-error 'namespace-variable-value
                          "(or/c #f (procedure-arity-includes/c 0))"
                          failure-thunk))
  (check 'namespace-variable-value namespace? ns)
  ((let/ec escape
     (define-values (var-ns var-phase-level var-sym)
       (cond
        [use-mapping?
         (define id (datum->syntax #f sym))
         (define b (resolve+shift/extra-inspector (namespace-syntax-introduce id ns)
                                                  (namespace-phase ns)
                                                  ns))
         (when b (namespace-visit-available-modules! ns))
         (define-values (v primitive? extra-inspector)
           (if b
               (binding-lookup b empty-env null ns (namespace-phase ns) id)
               (values variable #f #f)))
         (unless (variable? v)
           (escape
            (or failure-thunk
                (lambda ()
                  (raise (exn:fail:syntax 
                          (format (string-append "namespace-variable-value: bound to syntax\n"
                                                 "  in: ~s")
                                  sym)
                          (current-continuation-marks)
                          null))))))
         (if (module-binding? b)
             (values (if (top-level-module-path-index? (module-binding-module b))
                         ns
                         (module-instance-namespace (binding->module-instance b ns (namespace-phase ns) id)))
                     (module-binding-phase b)
                     (module-binding-sym b))
             (values ns (namespace-phase ns) sym))]
        [else
         (values ns (namespace-phase ns) sym)]))
     (define val
       (namespace-get-variable var-ns var-phase-level var-sym
                               (lambda () (escape
                                      (or failure-thunk
                                          (raise (exn:fail:contract:variable
                                                  (format (string-append
                                                           "namespace-variable-value: given name is not defined\n"
                                                           "  name: ~s")
                                                          sym)
                                                  (current-continuation-marks)
                                                  sym)))))))
     (lambda () val))))

(define (namespace-set-variable-value! sym	 	 	 	 
                                       val
                                       [map? #f]
                                       [ns (current-namespace)]
                                       [as-constant? #f])
  (check 'namespace-variable-value symbol? sym)
  (check 'namespace-variable-value namespace? ns)
  (namespace-set-variable! ns (namespace-phase ns) sym val as-constant?)
  (when map?
    (namespace-unset-transformer! ns (namespace-phase ns) sym)
    (define id (datum->syntax #f sym))
    (add-binding! (namespace-syntax-introduce id ns)
                  (make-module-binding (namespace-mpi ns)
                                       (namespace-phase ns)
                                       sym)
                  (namespace-phase ns))))

(define (namespace-undefine-variable! sym	 	 	 	 
                                      [ns (current-namespace)])
  (check 'namespace-variable-value symbol? sym)
  (check 'namespace-variable-value namespace? ns)
  (namespace-unset-variable! ns (namespace-phase ns) sym))

(define (namespace-mapped-symbols [ns (current-namespace)])
  (check 'namespace-mapped-symbols namespace? ns)
  (set->list
   (set-union
    (syntax-mapped-names (root-expand-context-all-scopes-stx (namespace-get-root-expand-ctx ns))
                         (namespace-phase ns))
    (list->set
     (instance-variable-names (namespace->instance ns 0))))))

(define (namespace-base-phase [ns (current-namespace)])
  (check 'namespace-base-phase namespace? ns)
  (namespace-phase ns))
