#lang racket/base
(require racket/set
         compiler/cm
         racket/file
         "find.rkt"
         "name.rkt"
         "merge.rkt"
         "gc.rkt"
         "bundle.rkt"
         "write.rkt")

(provide demodularize

         garbage-collect-toplevels-enabled
         current-excluded-modules
         recompile-enabled
         current-work-directory
         syntax-object-preservation-enabled
         current-maximum-phase
         current-merged-output-file)

(define garbage-collect-toplevels-enabled (make-parameter #f))
(define recompile-enabled (make-parameter 'auto))
(define current-work-directory (make-parameter #f))
(define syntax-object-preservation-enabled (make-parameter #f))

(define logger (make-logger 'demodularizer (current-logger)))

(define (demodularize input-file [given-output-file #f])
  (define given-work-directory (current-work-directory))
  (define work-directory (and (or (not (recompile-enabled))
                                  (not (eq? 'racket (system-type 'vm)))
                                  (syntax-object-preservation-enabled))
                              (or given-work-directory
                                  (make-temporary-file "demod-work-~a" 'directory))))

  (parameterize ([current-logger logger]
                 [current-excluded-modules (for/set ([path (in-set (current-excluded-modules))])
                                             (normal-case-path (simplify-path (path->complete-path path))))])

    (cond
      [work-directory
       (log-info "Compiling modules to ~s" work-directory)
       (parameterize ([current-namespace (make-empty-namespace)]
                      [current-compiled-file-roots (list (build-path work-directory "native")
                                                         (build-path work-directory "linklet"))]
                      [current-compile-target-machine #f]
                      [current-multi-compile-any #t])
         (namespace-attach-module (variable-reference->namespace (#%variable-reference)) ''#%builtin)
         (managed-compile-zo input-file))]
      [else
       (log-info "Compiling module")
       (parameterize ([current-namespace (make-base-empty-namespace)])
         (managed-compile-zo input-file))])

    (log-info "Finding modules")
    (define-values (phase-runs excluded-modules-to-require excluded-module-mpis provides)
      (parameterize ([current-compiled-file-roots (if work-directory
                                                      (list (build-path work-directory "linklet"))
                                                      (current-compiled-file-roots))])
        (find-modules input-file
                      #:keep-syntax? (syntax-object-preservation-enabled)
                      #:all-phases? (syntax-object-preservation-enabled))))

    (when (and work-directory (not given-work-directory))
      (delete-directory/files work-directory))

    (log-info "Selecting names")
    (define-values (names phase-internals phase-lifts phase-imports) (select-names phase-runs))

    (log-info "Merging linklets")
    (define-values (phase-body phase-first-internal-pos phase-merged-internals linkl-mode phase-import-keys
                               portal-stxes phase-defined-names
                               get-merge-info)
      (merge-linklets phase-runs names phase-internals phase-lifts phase-imports))

    (log-info "GCing definitions")
    (define-values (phase-new-body phase-new-internals phase-new-lifts phase-new-defined-names)
      (cond
        [(syntax-object-preservation-enabled)
         ;; any definition might be referenced reflectively
         (values phase-body phase-internals phase-lifts phase-defined-names)]
        [else
         (gc-definitions linkl-mode phase-body phase-internals phase-lifts phase-first-internal-pos phase-merged-internals
                         phase-defined-names
                         #:keep-defines? (syntax-object-preservation-enabled)
                         #:assume-pure? (garbage-collect-toplevels-enabled))]))

    (log-info "Bundling linklet")
    (define bundle (wrap-bundle linkl-mode phase-new-body phase-new-internals phase-new-lifts phase-import-keys
                                portal-stxes phase-new-defined-names
                                excluded-modules-to-require excluded-module-mpis provides names
                                get-merge-info
                                (let-values ([(base name dir?) (split-path input-file)])
                                  (string->symbol (path->string name)))
                                #:export? (syntax-object-preservation-enabled)))

    (log-info "Writing bytecode")
    (define output-file (or given-output-file
                            (path-add-suffix input-file #"_merged.zo")))
    (write-module output-file bundle)

    (when (or (eq? (recompile-enabled) #t)
              (and (eq? (recompile-enabled) 'auto)
                   (eq? linkl-mode 's-exp)))
      (log-info "Recompiling and rewriting bytecode")
      (define zo (compiled-expression-recompile
                  (parameterize ([read-accept-compiled #t])
                    (call-with-input-file* output-file read))))
      (call-with-output-file* output-file
                              #:exists 'replace
                              (lambda (out) (write zo out))))))
