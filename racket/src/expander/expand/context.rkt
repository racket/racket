#lang racket/base
(require "../common/promise.rkt"
         "../common/struct-star.rkt"
         "../common/parameter-like.rkt"
         "../syntax/syntax.rkt"
         "../syntax/scope.rkt"
         "../syntax/binding.rkt"
         "../syntax/error.rkt"
         "env.rkt"
         "free-id-set.rkt"
         "../namespace/namespace.rkt"
         "root-expand-context.rkt"
         "lift-key.rkt")

(provide (struct*-out expand-context)
         (all-from-out "root-expand-context.rkt")
         make-expand-context
         copy-root-expand-context
         current-expand-context
         get-current-expand-context

         current-expand-observe
         
         as-expression-context
         as-begin-expression-context
         as-tail-context
         as-named-context
         as-to-parsed-context)

;; An `expand-context` controls the process and result of expansion.
;;
;; If `to-parsed?` is true, the result is a `parsed` record instead of
;; an expanded syntax objects. That mode is effectively a fusion of
;; expansion and parsing, which is useful in the common case that
;; expanded code is being sent directly the the compiler.
;;
;; If only-immediate?` is set, then only immediate macro uses are
;; expanded. That mode overrides `to-parsed?`, since it's common to
;; partially expand forms on the way to a parsed result.

(struct* expand-context root-expand-context
         (to-parsed? ; #t => "expand" to a parsed form; #f => normal expand
          * context    ; 'expression, 'module, or 'top-level
          phase      ; current expansion phase; must match phase of `namespace`
          namespace  ; namespace for modules and evaluation
          * env        ; environment for local bindings
          * scopes     ; list of scopes that should be pruned by `quote-syntax`
          * def-ctx-scopes ; #f or box of list of scopes; transformer-created def-ctxes
          * binding-layer ; changed when a binding is nested; to check already-expanded
          * reference-records ; list of reference records for enclosing
          * only-immediate? ; #t => stop at core forms; #t => `def-ctx-scopes` is a box
          just-once? ; #t => stop (a given subform) after any expansion
          module-begin-k ; expander for `#%module-begin` in a 'module-begin context
          * need-eventually-defined ; phase(>=1) -> variables expanded before binding
          allow-unbound? ; allow reference to unbound identifiers as variables
          in-local-expand? ; #t via `local-expand`
          keep-#%expression? ; if `in-local-expand?`, keep `#%expression` forms
          stops      ; free-id-set; non-empty => `def-ctx-scopes` is a box
          * current-introduction-scopes ; scopes for current macro expansion
          * current-use-scopes ; scopes for current macro expansion
          declared-submodule-names ; mutable hash table: symbol -> 'module or 'module*
          lifts      ; #f or lift-context, which contains a list of lifteds
          lift-envs  ; list of box of env for lifts to locals
          module-lifts    ; lifted `module`s
          require-lifts   ; lifted `require`s
          to-module-lifts ; lifted `provide` and end declarations
          requires+provides ; enclosing module's requires+provides during `provide`
          * name       ; #f or identifier to name the expression
          observer   ; logging observer (for the macro debugger)
          for-serializable? ; accumulate submodules as serializable?
          to-correlated-linklet? ; compile to machine-independent linklets?
          normalize-locals? ; forget original local-variable names
          should-not-encounter-macros?)) ; #t when "expanding" to parse

(define (make-expand-context ns
                             #:to-parsed? [to-parsed? #f]
                             #:for-serializable? [for-serializable? #f]
                             #:to-correlated-linklet? [to-correlated-linklet? #f]
                             #:observer [observer #f])
  (define root-ctx (namespace-get-root-expand-ctx ns))
  (expand-context (root-expand-context-self-mpi root-ctx)
                  (root-expand-context-module-scopes root-ctx)
                  (root-expand-context-post-expansion root-ctx)
                  (root-expand-context-top-level-bind-scope root-ctx)
                  (root-expand-context-all-scopes-stx root-ctx)
                  (root-expand-context-use-site-scopes root-ctx)
                  (root-expand-context-defined-syms root-ctx)
                  (root-expand-context-frame-id root-ctx)
                  (root-expand-context-counter root-ctx)
                  (root-expand-context-lift-key root-ctx)
                  to-parsed?
                  'top-level
                  (namespace-phase ns)
                  ns
                  empty-env
                  null ; scopes
                  #f   ; def-ctx-scopes [=> don't record scopes to be stipped for `quote-syntax`]
                  (root-expand-context-frame-id root-ctx) ; binding-layer
                  null ; reference-records
                  #f   ; only-immediate?
                  #f   ; just-once?
                  #f   ; module-begin-k
                  #f   ; need-eventually-defined
                  #t   ; allow-unbound?
                  #f   ; in-local-expand?
                  #f   ; keep-#%expression?
                  empty-free-id-set ; stops
                  null ; current-introduction-scopes
                  null ; current-use-scopes
                  #hasheq() ; declared-submodule-names
                  #f   ; lifts
                  '()  ; lift-envs
                  #f   ; module-lifts
                  #f   ; require-lifts
                  #f   ; to-module-lifts
                  #f   ; requires+provides
                  #f   ; name
                  observer
                  for-serializable?
                  to-correlated-linklet?
                  to-correlated-linklet? ; normalize-locals?
                  #f))

(define (copy-root-expand-context ctx root-ctx)
  (struct*-copy expand-context ctx
                [self-mpi #:parent root-expand-context (root-expand-context-self-mpi root-ctx)]
                [module-scopes #:parent root-expand-context (root-expand-context-module-scopes root-ctx)]
                [post-expansion #:parent root-expand-context (root-expand-context-post-expansion root-ctx)]
                [top-level-bind-scope #:parent root-expand-context (root-expand-context-top-level-bind-scope root-ctx)]
                [all-scopes-stx #:parent root-expand-context (root-expand-context-all-scopes-stx root-ctx)]
                [use-site-scopes #:parent root-expand-context (root-expand-context-use-site-scopes root-ctx)]
                [defined-syms #:parent root-expand-context (root-expand-context-defined-syms root-ctx)]
                [frame-id #:parent root-expand-context (root-expand-context-frame-id root-ctx)]
                [counter #:parent root-expand-context (root-expand-context-counter root-ctx)]
                [lift-key #:parent root-expand-context (root-expand-context-lift-key root-ctx)]
                [binding-layer (root-expand-context-frame-id root-ctx)]))

;; An expand-context or a delayed expand context (so use `force`):
(define-parameter-like current-expand-context #f)

(define (get-current-expand-context [who 'unexpected]
                                     #:fail-ok? [fail-ok? #f])
  (or (force (current-expand-context))
      (if fail-ok?
          #f
          (raise-arguments-error who "not currently expanding"))))

;; ----------------------------------------

;; For macro debugging. This parameter is only used by the expander
;; entry points in "../eval/main.rkt" to set the expand-context
;; observer. Other expander code uses "log.rkt" to send expansion
;; events to the observer.
(define current-expand-observe (make-parameter #f
                                               (lambda (v)
                                                 (unless (or (not v)
                                                             (and (procedure? v)
                                                                  (procedure-arity-includes? v 2)))
                                                   (raise-argument-error 'current-expand-observe
                                                                          "(or/c (procedure-arity-includes/c 2) #f)"
                                                                          v))
                                                 v)))

;; ----------------------------------------

;; Adjusts `ctx` to make it suitable for a subexpression of the
;; current context
(define (as-expression-context ctx)
  (cond
   [(and (eq? 'expression (expand-context-context ctx))
         (not (expand-context-name ctx)))
    ctx]
   [else (struct*-copy expand-context ctx
                       [context 'expression]
                       [name #f]
                       [post-expansion #:parent root-expand-context #f])]))

;; Adjusts `ctx` to make it suitable for a non-tail position
;; in an `begin` form, possibly in a 'top-level or 'module context
;; (so don't force it to 'expression mode)
(define (as-begin-expression-context ctx)
  (cond
   [(not (expand-context-name ctx))
    ctx]
   [else (struct*-copy expand-context ctx
                       [name #f])]))

;; Adjusts `ctx` (which should be an expression context) to make it
;; suitable for a subexpression in tail position
(define (as-tail-context ctx #:wrt wrt-ctx)
  (cond
   [(expand-context-name wrt-ctx)
    (struct*-copy expand-context ctx
                  [name (expand-context-name wrt-ctx)])]
   [else ctx]))

;; Adjust `ctx` to make it suitable for a context in the right-hand
;; side of a definition of `ids`
(define (as-named-context ctx ids)
  (cond
   [(and (pair? ids) (null? (cdr ids)))
    (struct*-copy expand-context ctx
                  [name (car ids)])]
   [else ctx]))

;; Adjust `ctx` to to generate a parsed result
(define (as-to-parsed-context ctx)
  (struct*-copy expand-context ctx
                [to-parsed? #t]
                [observer #f]
                [should-not-encounter-macros? #t]))

;; ----------------------------------------

;; Register a callback for `raise-syntax-error`
(set-current-previously-unbound!
 (lambda ()
   (define ctx (force (current-expand-context)))
   (define phase-to-ids (and ctx (expand-context-need-eventually-defined ctx)))
   (and phase-to-ids
        (hash-ref phase-to-ids (expand-context-phase ctx) null))))
