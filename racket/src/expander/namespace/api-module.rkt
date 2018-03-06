#lang racket/base
(require "../common/module-path.rkt"
         "../expand/root-expand-context.rkt"
         "namespace.rkt"
         (submod "namespace.rkt" for-module)
         "module.rkt"
         "provide-for-api.rkt"
         "provided.rkt"
         (submod "module.rkt" for-module-reflect)
         "../common/contract.rkt")

(provide module-declared?
         module-predefined?
         module->language-info
         module->imports
         module->exports
         module->indirect-exports
         module-provide-protected?
         module->namespace
         namespace-unprotect-module)

;; ----------------------------------------

(define/who (module-declared? mod [load? #f])
  (check who module-reference? #:contract module-reference-str mod)
  (define ns (current-namespace))
  (define name (reference->resolved-module-path mod #:load? load?))
  (and (namespace->module ns name) #t))

(define/who (module-predefined? mod)
  (check who module-reference? #:contract module-reference-str mod)
  (define ns (current-namespace))
  (define name (reference->resolved-module-path mod #:load? #f))
  (define m (namespace->module ns name))
  (and m (module-is-predefined? m)))

(define (module-> extract who mod [load? #f])
  (check who module-reference? #:contract module-reference-str mod)
  (define m (namespace->module/complain who
                                        (current-namespace)
                                        (reference->resolved-module-path mod #:load? load?)))
  (extract m))

(define/who (module->language-info mod [load? #f])
  (module-> module-language-info who mod load?))

(define/who (module->imports mod)
  (module-> module-requires who mod))

(define (module->exports mod)
  (define-values (provides self)
    (module-> (lambda (m) (values (module-provides m) (module-self m))) 'module->exports mod))
  (provides->api-provides provides self))

(define (module->indirect-exports mod)
  (module-> (lambda (m)
              (variables->api-nonprovides (module-provides m)
                                          ((module-get-all-variables m))))
            'module->indirect-exports mod))

(define (module-provide-protected? mod sym)
  (module-> (lambda (m)
              (define b/p (hash-ref (module-provides m) sym #f))
              (or (not b/p) (provided-as-protected? b/p)))
            'module-provide-protected? mod))

(define/who (module->namespace mod [ns (current-namespace)])
  (check who module-reference? #:contract module-reference-str mod)
  (check who namespace? ns)
  (define name (reference->resolved-module-path mod #:load? #t))
  (define phase (namespace-phase ns))
  (define m-ns (namespace->module-namespace ns name phase))
  (unless m-ns
    ;; Check for declaration:
    (namespace->module/complain 'module->namespace ns name)
    ;; Must be declared, but not instantiated
    (raise-arguments-error who
                           "module not instantiated in the current namespace"
                           "name" name))
  (unless (inspector-superior? (current-code-inspector) (namespace-inspector m-ns))
    (raise-arguments-error who
                           "current code inspector cannot access namespace of module"
                           "module name" name))
  (unless (namespace-get-root-expand-ctx m-ns)
    ;; Instantiating the module didn't install a context, so make one now
    (namespace-set-root-expand-ctx! m-ns (make-root-expand-context
                                          #:self-mpi (namespace-mpi m-ns))))
  ;; Ensure that the module is available
  (namespace-module-make-available! ns (namespace-mpi m-ns) phase)
  m-ns)

(define/who (namespace-unprotect-module insp mod [ns (current-namespace)])
  (check who inspector? insp)
  (check who module-path? mod)
  (check who namespace? ns)
  (define name (reference->resolved-module-path mod #:load? #f))
  (define phase (namespace-phase ns))
  (define m-ns (namespace->module-namespace ns name phase))
  (unless m-ns
    (raise-arguments-error who
                           "module not instantiated"
                           "module name" name))
  (when (inspector-superior? insp (namespace-inspector m-ns))
    (set-namespace-inspector! m-ns (make-inspector (current-code-inspector)))))

;; ----------------------------------------

(define (namespace->module/complain who ns name)
  (or (namespace->module ns name)
      (raise-arguments-error who
                             "unknown module in the current namespace"
                             "name" name)))

;; ----------------------------------------
  
(define (module-reference? mod)
  (or (module-path? mod)
      (module-path-index? mod)
      (resolved-module-path? mod)))

(define module-reference-str
  "(or/c module-path? module-path-index? resolved-module-path?)")

(define (reference->resolved-module-path mod #:load? load?)
  (cond
   [(resolved-module-path? mod) mod]
   [else
    (define mpi (if (module-path-index? mod)
                    mod
                    (module-path-index-join mod #f)))
    (module-path-index-resolve mpi load?)]))
