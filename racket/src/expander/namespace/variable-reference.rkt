#lang racket/base
(require "namespace.rkt"
         "../common/contract.rkt"
         "../common/module-path.rkt"
         "../host/linklet.rkt"
         "module.rkt"
         "api-module.rkt")

(provide variable-reference?          ; provided by linklet layer, along with `#%variable-reference`
         variable-reference-constant? ; provided by linklet layer
         variable-reference-from-unsafe? ; provided by linklet layer

         variable-reference->empty-namespace
         variable-reference->namespace
         variable-reference->module-path-index
         variable-reference->resolved-module-path
         variable-reference->module-source
         variable-reference->phase
         variable-reference->module-base-phase
         variable-reference->module-declaration-inspector
         variable-reference-add-attach-dependency!)

(define/who (variable-reference->empty-namespace vr)
  (check who variable-reference? vr)
  (new-namespace (variable-reference->namespace vr)))

(define/who (variable-reference->namespace vr)
  (check who variable-reference? vr)
  (define ns (variable-reference->namespace* who vr))
  (define mpi (namespace-mpi ns))
  (when (non-self-module-path-index? mpi)
    ;; Ensure that the module is available
    (parameterize ([current-namespace ns])
      (namespace-module-make-available! ns mpi (namespace-0-phase ns))))
  ns)

(define (check-got-ns who vr maybe-ns)
  (unless (namespace? maybe-ns)
    (raise-arguments-error who
                           "variable reference has no associated namespace"
                           "variable reference" vr
                           "hint" (unquoted-printing-string "is the variable reference from a non-module, non-top-level linklet?")))
  maybe-ns)

(define (variable-reference->namespace* who vr)
  (define inst (variable-reference->instance vr))
  (cond
    [(symbol? inst)
     ;; This case happens for `(#%variable-reference id)` where `id`
     ;; refers directly to a primitive. We get a namespace for a
     ;; primitive instance; that might not be the same module as
     ;; reorted by `identifier-binding`, but close enough.
     (module->namespace `',inst (check-got-ns who vr (instance-data (variable-reference->instance vr #t))))]
    [(not inst)
     ;; Anonymous variable reference; use the referencing namespace
     (check-got-ns who vr (instance-data (variable-reference->instance vr #t)))]
    [else
     ;; Get the defining namespace for the referenced variable
     (check-got-ns who vr (instance-data inst))]))

(define/who (variable-reference->module-path-index vr)
  (check who variable-reference? vr)
  (define mpi (namespace-mpi (variable-reference->namespace* who vr)))
  (if (top-level-module-path-index? mpi)
      #f
      mpi))

(define/who (variable-reference->resolved-module-path vr)
  (check who variable-reference? vr)
  (define mpi (variable-reference->module-path-index vr))
  (and mpi (module-path-index-resolve mpi)))

(define/who (variable-reference->module-source vr)
  (check who variable-reference? vr)
  (define ns (variable-reference->namespace* who vr))
  (namespace-source-name ns))

(define/who (variable-reference->phase vr)
 (check who variable-reference? vr)
 (namespace-phase (variable-reference->namespace* who vr)))

(define/who (variable-reference->module-base-phase vr)
  (check who variable-reference? vr)
  (namespace-0-phase (variable-reference->namespace* who vr)))

(define/who (variable-reference->module-declaration-inspector vr)
  (check who variable-reference? vr)
  (when (variable-reference->instance vr)
    (raise-arguments-error who
                           "variable reference does not refer to an anonymous module variable"
                           "variable reference" vr))
  (or (namespace-declaration-inspector (variable-reference->namespace* who vr))
      (raise-arguments-error who
                             "given variable reference is not from a module")))

;; Records that the module instance associated with `vr` may load the
;; module referenced by `mpi` lazily (as `lazy-require` does). That
;; matters only for a cross-phase persistent module: since the instance
;; is shared with any namespace that it is attached to, but the lazily
;; loaded module would be loaded in the instance's original namespace,
;; `namespace-attach-module` first loads and instantiates the referenced
;; module and then attaches it along with the instance
(define/who (variable-reference-add-attach-dependency! vr mpi)
  (check who variable-reference? vr)
  (check who module-path-index? mpi)
  (define ns (variable-reference->namespace* who vr))
  (define mod-mpi (namespace-mpi ns))
  (unless (top-level-module-path-index? mod-mpi)
    (define mi (namespace->module-instance ns
                                           (module-path-index-resolve mod-mpi)
                                           (namespace-0-phase ns)))
    (define m (and mi (module-instance-module mi)))
    (when (and m (module-cross-phase-persistent? m))
      (define deps (module-instance-attach-dependencies mi))
      (unless (member mpi deps)
        (set-module-instance-attach-dependencies! mi (cons mpi deps))))))
