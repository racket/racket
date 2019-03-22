#lang racket/base
(require "../common/set.rkt"
         "../common/struct-star.rkt"
         "../syntax/syntax.rkt"
         "../common/phase.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/taint.rkt"
         "env.rkt"
         "context.rkt"
         "main.rkt"
         "../namespace/core.rkt"
         "use-site.rkt"
         "rename-trans.rkt"
         "lift-context.rkt"
         "require.rkt"
         "require+provide.rkt"
         "protect.rkt"
         "log.rkt"
         "module-path.rkt"
         "definition-context.rkt"
         "../common/module-path.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../common/contract.rkt")

(provide syntax-transforming?
         syntax-transforming-with-lifts?
         syntax-transforming-module-expression?
         syntax-local-transforming-module-provides?
         
         syntax-local-context
         syntax-local-introduce
         syntax-local-identifier-as-binding
         syntax-local-phase-level
         syntax-local-name

         make-syntax-introducer
         make-interned-syntax-introducer
         make-syntax-delta-introducer
         syntax-local-make-delta-introducer
         
         syntax-local-value
         syntax-local-value/immediate
         
         syntax-local-lift-expression
         syntax-local-lift-values-expression
         syntax-local-lift-context
         
         syntax-local-lift-module
         
         syntax-local-lift-require
         syntax-local-lift-provide
         syntax-local-lift-module-end-declaration
         
         syntax-local-module-defined-identifiers
         syntax-local-module-required-identifiers
         syntax-local-module-exports
         syntax-local-submodules
         
         syntax-local-get-shadower)

;; ----------------------------------------

(define (syntax-transforming?)
  (and (get-current-expand-context #:fail-ok? #t) #t))

(define (syntax-transforming-with-lifts?)
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (and ctx
       (expand-context-lifts ctx)
       #t))

(define (syntax-transforming-module-expression?)
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (and ctx
       (expand-context-to-module-lifts ctx)
       #t))

(define (syntax-local-transforming-module-provides?)
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (and ctx
       (expand-context-requires+provides ctx)
       #t))
  
;; ----------------------------------------

(define (syntax-local-context)
  (define ctx (get-current-expand-context 'syntax-local-context))
  (expand-context-context ctx))

(define/who (syntax-local-introduce s)
  (check who syntax? s)
  (define ctx (get-current-expand-context 'syntax-local-introduce))
  (flip-introduction-and-use-scopes s ctx))

(define/who (syntax-local-identifier-as-binding id)
  (check who identifier? id)
  (define ctx (get-current-expand-context 'syntax-local-identifier-as-binding))
  (remove-use-site-scopes id ctx))

(define (syntax-local-phase-level)
  (define ctx (get-current-expand-context #:fail-ok? #t))
  (if ctx
      (expand-context-phase ctx)
      0))

(define/who (syntax-local-name)
  (define ctx (get-current-expand-context who))
  (define id (expand-context-name ctx))
  (and id
       ;; Strip lexical context, but keep source-location information
       (datum->syntax #f (syntax-e id) id)))

;; ----------------------------------------

(define (make-syntax-introducer [as-use-site? #f])
  (do-make-syntax-introducer (new-scope (if as-use-site? 'use-site 'macro))))

(define/who (make-interned-syntax-introducer sym-key)
  (check who symbol? sym-key)
  (do-make-syntax-introducer (make-interned-scope sym-key)))

(define (do-make-syntax-introducer sc)
  (lambda (s [mode 'flip])
    (check 'syntax-introducer syntax? s)
    (case mode
      [(add) (add-scope s sc)]
      [(remove) (remove-scope s sc)]
      [(flip) (flip-scope s sc)]
      [else (raise-argument-error 'syntax-introducer "(or/c 'add 'remove 'flip)" mode)])))

(define/who (make-syntax-delta-introducer ext-s base-s [phase (syntax-local-phase-level)])
  (check who syntax? ext-s)
  (check who syntax? #:or-false base-s)
  (check who phase? #:contract phase?-string phase)
  (define ext-scs (syntax-scope-set ext-s phase))
  (define base-scs (syntax-scope-set (or base-s empty-syntax) phase))
  (define use-base-scs (if (subset? base-scs ext-scs)
                           base-scs
                           (or (and (identifier? base-s)
                                    (resolve base-s phase #:get-scopes? #t))
                               (seteq))))
  (define delta-scs (set->list (set-subtract ext-scs use-base-scs)))
  (define maybe-taint (if (syntax-clean? ext-s) values syntax-taint))
  (define shifts (syntax-mpi-shifts ext-s))
  (lambda (s [mode 'add])
    (maybe-taint
     (case mode
       [(add) (syntax-add-shifts (add-scopes s delta-scs) shifts #:non-source? #t)]
       [(remove) (remove-scopes s delta-scs)]
       [(flip) (syntax-add-shifts (flip-scopes s delta-scs) shifts #:non-source? #t)]
       [else (raise-argument-error 'syntax-introducer "(or/c 'add 'remove 'flip)" mode)]))))

(define/who (syntax-local-make-delta-introducer id-stx)
  (check who identifier? id-stx)
  (raise
   (exn:fail:unsupported "syntax-local-make-delta-introducer: not supported anymore"
                         (current-continuation-marks))))

;; ----------------------------------------

(define (do-syntax-local-value who id intdefs failure-thunk
                               #:immediate? immediate?)
  (check who identifier? id)
  (check who #:or-false (procedure-arity-includes/c 0) failure-thunk)
  (check who intdefs-or-false? #:contract intdefs-or-false?-string intdefs)
  (define current-ctx (get-current-expand-context who))
  (define ctx (if intdefs
                  (struct*-copy expand-context current-ctx
                                [env (add-intdef-bindings (expand-context-env current-ctx)
                                                          intdefs)])
                  current-ctx))
  (log-expand ctx 'local-value id)
  (define phase (expand-context-phase ctx))
  (let loop ([id (flip-introduction-scopes id ctx)])
    (define b (if immediate?
                  (resolve+shift id phase #:immediate? #t)
                  (resolve+shift/extra-inspector id phase (expand-context-namespace ctx))))
    (log-expand ctx 'resolve id)
    (cond
     [(not b)
      (log-expand ctx 'local-value-result #f)
      (if failure-thunk
          (failure-thunk)
          (error who "unbound identifier: ~v" id))]
     [else
      (define-values (v primitive? insp protected?)
        (lookup b ctx id #:out-of-context-as-variable? #t))
      (cond
       [(or (variable? v) (core-form? v))
        (log-expand ctx 'local-value-result #f)
        (if failure-thunk
            (failure-thunk)
            (error who "identifier is not bound to syntax: ~v" id))]
       [else
        (log-expand* ctx #:unless (and (rename-transformer? v) (not immediate?))
                     ['local-value-result #t])
        (cond
         [(rename-transformer? v)
          (if immediate?
              (values v (rename-transformer-target v))
              (loop (rename-transformer-target v)))]
         [immediate? (values v #f)]
         [else v])])])))

(define (syntax-local-value id [failure-thunk #f] [intdef #f])
  (do-syntax-local-value 'syntax-local-value #:immediate? #f id intdef failure-thunk))

(define (syntax-local-value/immediate id [failure-thunk #f] [intdef #f])
  (do-syntax-local-value 'syntax-local-value/immediate #:immediate? #t id intdef failure-thunk))

;; ----------------------------------------

(define (do-lift-values-expression who n s)
  (check who syntax? s)
  (check who exact-nonnegative-integer? n)
  (define ctx (get-current-expand-context who))
  (define lifts (expand-context-lifts ctx))
  (unless lifts (raise-arguments-error who "no lift target"))
  (define counter (root-expand-context-counter ctx))
  (define ids (for/list ([i (in-range n)])
                (set-box! counter (add1 (unbox counter)))
                (define name (string->unreadable-symbol (format "lifted/~a" (unbox counter))))
                (add-scope (datum->syntax #f name) (new-scope 'macro))))
  (log-expand ctx 'lift-expr ids s)
  (map (lambda (id) (flip-introduction-scopes id ctx))
       ;; returns converted ids:
       (add-lifted! lifts
                    ids
                    (flip-introduction-scopes s ctx)
                    (expand-context-phase ctx))))

(define/who (syntax-local-lift-expression s)
  (car (do-lift-values-expression who 1 s)))

(define/who (syntax-local-lift-values-expression n s)
  (do-lift-values-expression who n s))

(define/who (syntax-local-lift-context)
  (define ctx (get-current-expand-context who))
  (root-expand-context-lift-key ctx))

;; ----------------------------------------

(define/who (syntax-local-lift-module s)
  (check who syntax? s)
  (define ctx (get-current-expand-context who))
  (define phase (expand-context-phase ctx))
  (case (core-form-sym s phase)
    [(module module*)
     (define lifts (expand-context-module-lifts ctx))
     (unless lifts
       (raise-arguments-error who
                              "not currently transforming within a module declaration or top level"
                              "form to lift" s))
     (add-lifted-module! lifts (flip-introduction-scopes s ctx) phase)]
    [else
     (raise-arguments-error who "not a module form"
                            "given form" s)])
  (log-expand ctx 'lift-statement s))

;; ----------------------------------------

(define (do-local-lift-to-module who s
                                 #:no-target-msg no-target-msg
                                 #:intro? [intro? #t]
                                 #:more-checks [more-checks void]
                                 #:get-lift-ctx get-lift-ctx
                                 #:add-lifted! add-lifted!
                                 #:get-wrt-phase get-wrt-phase
                                 #:pre-wrap [pre-wrap (lambda (s phase lift-ctx) s)]
                                 #:shift-wrap [shift-wrap (lambda (s phase lift-ctx) s)]
                                 #:post-wrap [post-wrap (lambda (s phase lift-ctx) s)])
  (check who syntax? s)
  (more-checks)
  (define ctx (get-current-expand-context who))
  (define lift-ctx (get-lift-ctx ctx))
  (unless lift-ctx (raise-arguments-error who no-target-msg
                                          "form to lift" s))
  (define phase (expand-context-phase ctx))   ; we're currently at this phase
  (define wrt-phase (get-wrt-phase lift-ctx)) ; lift context is at this phase
  (define added-s (if intro? (flip-introduction-scopes s ctx) s))
  (define pre-s (pre-wrap added-s phase lift-ctx)) ; add pre-wrap at current phase
  (define shift-s (for/fold ([s pre-s]) ([phase (in-range phase wrt-phase -1)]) ; shift from lift-context phase
                    (shift-wrap s (sub1 phase) lift-ctx)))
  (define post-s (post-wrap shift-s wrt-phase lift-ctx)) ; post-wrap at lift-context phase
  (add-lifted! lift-ctx post-s wrt-phase) ; record lift for the target phase
  (values ctx post-s))

(define/who (syntax-local-lift-require s use-s)
  (define sc (new-scope 'lifted-require))
  (define-values (ctx added-s)
    (do-local-lift-to-module who
                             (datum->syntax #f s)
                             #:no-target-msg "could not find target context"
                             #:intro? #f
                             #:more-checks
                             (lambda ()
                               (check who syntax? use-s))
                             #:get-lift-ctx expand-context-require-lifts
                             #:get-wrt-phase require-lift-context-wrt-phase
                             #:add-lifted! add-lifted-require!
                             #:shift-wrap
                             (lambda (s phase require-lift-ctx)
                               (require-spec-shift-for-syntax s))
                             #:post-wrap
                             (lambda (s phase require-lift-ctx)
                               (wrap-form '#%require (add-scope s sc) phase))))
  (namespace-visit-available-modules! (expand-context-namespace ctx)
                                      (expand-context-phase ctx))
  (define result-s (add-scope use-s sc))
  (log-expand ctx 'lift-require added-s use-s result-s)
  result-s)

(define/who (syntax-local-lift-provide s)
  (define-values (ctx result-s)
    (do-local-lift-to-module who
                             s
                             #:no-target-msg "not expanding in a module run-time body"
                             #:get-lift-ctx expand-context-to-module-lifts
                             #:get-wrt-phase to-module-lift-context-wrt-phase
                             #:add-lifted! add-lifted-to-module-provide!
                             #:shift-wrap
                             (lambda (s phase to-module-lift-ctx)
                               (wrap-form 'for-syntax s #f))
                             #:post-wrap
                             (lambda (s phase to-module-lift-ctx)
                               (wrap-form '#%provide s phase))))
  (log-expand ctx 'lift-provide result-s))

(define/who (syntax-local-lift-module-end-declaration s)
  (define-values (ctx also-s)
    (do-local-lift-to-module who
                             s
                             #:no-target-msg "not currently transforming an expression within a module declaration"
                             #:get-lift-ctx expand-context-to-module-lifts
                             #:get-wrt-phase (lambda (lift-ctx) 0) ; always relative to 0
                             #:add-lifted! add-lifted-to-module-end!
                             #:pre-wrap
                             (lambda (orig-s phase to-module-lift-ctx)
                               (if (to-module-lift-context-end-as-expressions? to-module-lift-ctx)
                                   (wrap-form '#%expression orig-s phase)
                                   orig-s))
                             #:shift-wrap
                             (lambda (s phase to-module-lift-ctx)
                               (wrap-form 'begin-for-syntax s phase))))
  (log-expand ctx 'lift-statement s))

(define (wrap-form sym s phase)
  (datum->syntax
   #f
   (list (datum->syntax
          (if phase
              (syntax-shift-phase-level core-stx phase)
              #f)
          sym)
         s)))

;; ----------------------------------------

(define/who (syntax-local-module-defined-identifiers)
  (unless (syntax-local-transforming-module-provides?)
    (raise-arguments-error who "not currently transforming module provides"))
  (define ctx (get-current-expand-context 'syntax-local-module-defined-identifiers))
  (requireds->phase-ht (extract-module-definitions (expand-context-requires+provides ctx))))
  
  
(define/who (syntax-local-module-required-identifiers mod-path phase-level)
  (unless (or (not mod-path) (module-path? mod-path))
    (raise-argument-error who "(or/c module-path? #f)" mod-path))
  (unless (or (eq? phase-level #t) (phase? phase-level))
    (raise-argument-error who (format "(or/c ~a #t)" phase?-string) phase-level))
  (unless (syntax-local-transforming-module-provides?)
    (raise-arguments-error who "not currently transforming module provides"))
  (define ctx (get-current-expand-context 'syntax-local-module-required-identifiers))
  (define requires+provides (expand-context-requires+provides ctx))
  (define mpi (and mod-path
                   (module-path->mpi/context mod-path ctx)))
  (define requireds
    (extract-all-module-requires requires+provides
                                 mpi
                                 (if (eq? phase-level #t) 'all phase-level)))
  (and requireds
       (for/list ([(phase ids) (in-hash (requireds->phase-ht requireds))])
         (cons phase ids))))

(define (requireds->phase-ht requireds)
  (for/fold ([ht (hasheqv)]) ([r (in-list requireds)])
    (hash-update ht
                 (required-phase r)
                 (lambda (l) (cons (required-id r) l))
                 null)))

;; ----------------------------------------

(define/who (syntax-local-module-exports mod-path)
  (unless (or (module-path? mod-path)
              (and (syntax? mod-path)
                   (module-path? (syntax->datum mod-path))))
    (raise-argument-error who
                          (string-append
                           "(or/c module-path?\n"
                           "      (and/c syntax?\n"
                           "             (lambda (stx)\n"
                           "               (module-path? (syntax->datum stx)))))")
                          mod-path))
  (define ctx (get-current-expand-context 'syntax-local-module-exports))
  (define ns (expand-context-namespace ctx))
  (define mod-name (module-path-index-resolve
                    (module-path->mpi/context (if (syntax? mod-path)
                                                  (syntax->datum mod-path)
                                                  mod-path)
                                              ctx)
                    #t))
  (define m (namespace->module ns mod-name))
  (unless m (raise-unknown-module-error 'syntax-local-module-exports mod-name))
  (for/list ([(phase syms) (in-hash (module-provides m))])
    (cons phase
          (for/list ([sym (in-hash-keys syms)])
            sym))))

(define/who (syntax-local-submodules)
  (define ctx (get-current-expand-context who))
  (define submods (expand-context-declared-submodule-names ctx))
  (for/list ([(name kind) (in-hash submods)]
             #:when (eq? kind 'module))
    name))

;; ----------------------------------------

;; Works well enough for some backward compatibility:
(define/who (syntax-local-get-shadower id [only-generated? #f])
  (check who identifier? id)
  (define ctx (get-current-expand-context who))
  (define new-id (add-scopes id (expand-context-scopes ctx)))
  (if (syntax-clean? id)
      new-id
      (syntax-taint new-id)))
