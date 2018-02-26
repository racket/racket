#lang racket/base
(require "namespace.rkt"
         "../common/contract.rkt"
         "../common/module-path.rkt"
         "../host/linklet.rkt"
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
         variable-reference->module-declaration-inspector)

(define (variable-reference->empty-namespace vr)
  (check 'variable-reference->empty-namespace variable-reference? vr)
  (new-namespace (variable-reference->namespace vr)))

(define (variable-reference->namespace vr)
  (check 'variable-reference->namespace variable-reference? vr)
  (define inst (variable-reference->instance vr))
  (cond
    [(symbol? inst)
     ;; This case happens for `(#%variable-reference id)` where `id`
     ;; refers directly to a primitive. The expander doesn't currently
     ;; generate that, but just in case... We get a namespace for a
     ;; primitive instance; that might not be the same module as
     ;; reorted by `identifier-binding`, but close enough.
     (module->namespace `',inst (instance-data (variable-reference->instance vr #t)))]
    [(not inst)
     ;; Anonymous variable reference; use the referencing namespace
     (instance-data (variable-reference->instance vr #t))]
    [else
     ;; Get the defining namespace for the referenced variable
     (instance-data inst)]))

(define (variable-reference->module-path-index vr)
  (check 'variable-reference->module-path-index variable-reference? vr)
  (define mpi (namespace-mpi (variable-reference->namespace vr)))
  (if (top-level-module-path-index? mpi)
      #f
      mpi))

(define (variable-reference->resolved-module-path vr)
  (check 'variable-reference->resolved-module-path variable-reference? vr)
  (define mpi (variable-reference->module-path-index vr))
  (and mpi (module-path-index-resolve mpi)))

(define (variable-reference->module-source vr)
  (check 'variable-reference->module-source variable-reference? vr)
  (define ns (variable-reference->namespace vr))
  (namespace-source-name ns))

(define (variable-reference->phase vr)
 (check 'variable-reference->phase variable-reference? vr)
 (namespace-phase (variable-reference->namespace vr)))

(define (variable-reference->module-base-phase vr)
  (check 'variable-reference->module-base-phase variable-reference? vr)
  (namespace-0-phase (variable-reference->namespace vr)))

(define (variable-reference->module-declaration-inspector vr)
  (check 'variable-reference->module-declaration-inspector variable-reference? vr)
  (when (variable-reference->instance vr)
    (raise-arguments-error 'variable-reference->module-declaration-inspector
                           "variable reference does not refer to an anonymous module variable"
                           "variable reference" vr))
  (or (namespace-declaration-inspector (variable-reference->namespace vr))
      (raise-arguments-error 'variable-reference->module-declaration-inspector
                             "given variable reference is not from a module")))
