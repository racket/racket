#lang racket/base
(require "../common/set.rkt"
         "../host/linklet.rkt"
         "../compile/module-use.rkt"
         "../common/module-path.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../compile/extra-inspector.rkt")

;; Inspectors guarded access to protected values at expansion time. We
;; run code that references portentially protected values, we have to
;; check again, in case the compiled form was synthesized or compiled
;; in a namespace with different protections.

;; When programs are compiled and run in memory (i.e., without going
;; through serialization), we can trust the protections checked by the
;; expander --- and the access enabled by compiling from source may be
;; greater than enabled by serialized bytecode, because inspectors can
;; be tracked as values and changed at a finer granularity. In that
;; case, a `compiled-in-memory` record holds extra-inspector
;; information that is propagated to here.

(provide check-require-access
         check-single-require-access)

(define (check-require-access linklet #:skip-imports skip-num-imports
                              import-module-uses import-module-instances insp
                              extra-inspector     ; from declaration time
                              extra-inspectorsss) ; per imported variable; from compilation
  (for ([import-syms (in-list (list-tail (linklet-import-variables linklet) skip-num-imports))]
        [mu (in-list import-module-uses)]
        [mi (in-list import-module-instances)]
        [extra-inspectorss (in-list (or extra-inspectorsss
                                        ;; Use `import-module-uses` just to have the right shape
                                        import-module-uses))])
    (define m (module-instance-module mi))
    (unless (module-no-protected? m)
      (define access (or (module-access m) (module-compute-access! m)))
      (for ([import-sym (in-list import-syms)])
        (define a (hash-ref (hash-ref access (module-use-phase mu) #hasheq())
                            import-sym
                            'unexported))
        (when (or (eq? a 'unexported) ; not provided => implicitly protected
                  (eq? a 'protected))
          (define guard-insp (namespace-inspector (module-instance-namespace mi)))
          (unless (or
                   ;; Allowed at declaration time?
                   (inspector-superior? insp guard-insp)
                   ;; Allowed back at compile time?
                   (and extra-inspector (inspector-superior? extra-inspector guard-insp))
                   ;; Allowed by inspectors attached to each referencing syntax object?
                   (and extra-inspectorsss
                        extra-inspectorss
                        (extra-inspectors-allow? (hash-ref extra-inspectorss import-sym #f)
                                                 guard-insp)))
            (error 'link
                   (string-append "access disallowed by code inspector to ~a variable\n"
                                  "  variable: ~s\n"
                                  "  from module: ~a")
                   a
                   import-sym
                   (module-path-index-resolve (namespace-mpi (module-instance-namespace mi))))))))))

(define (check-single-require-access mi phase sym insp)
  (define m (module-instance-module mi))
  (cond
   [(module-no-protected? m) #t]
   [else
    (define access (or (module-access m) (module-compute-access! m)))
    (define a
      (hash-ref (hash-ref access phase #hasheq())
                sym
                'unexported))
    (cond
     [(or (eq? a 'unexported) ; not provided => implicitly protected
          (eq? a 'protected))
      (define guard-insp (namespace-inspector (module-instance-namespace mi)))
      (or (and insp
               (inspector-superior? insp guard-insp))
          (inspector-superior? (current-code-inspector) guard-insp))]
     [else #t])]))
