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
         current-work-directory)

(define garbage-collect-toplevels-enabled (make-parameter #f))
(define recompile-enabled (make-parameter 'auto))
(define current-work-directory (make-parameter #f))

(define logger (make-logger 'demodularizer (current-logger)))

(define (demodularize input-file [given-output-file #f])
  (define given-work-directory (current-work-directory))
  (define work-directory (and (or (not (recompile-enabled))
                                  (not (eq? 'racket (system-type 'vm))))
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
    (define-values (runs excluded-module-mpis)
      (parameterize ([current-compiled-file-roots (if work-directory
                                                      (list (build-path work-directory "linklet"))
                                                      (current-compiled-file-roots))])
        (find-modules input-file)))

    (when (and work-directory (not given-work-directory))
      (delete-directory/files work-directory))

    (log-info "Selecting names")
    (define-values (names internals lifts imports) (select-names runs))

    (log-info "Merging linklets")
    (define-values (body first-internal-pos merged-internals linkl-mode get-merge-info)
      (merge-linklets runs names internals lifts imports))

    (log-info "GCing definitions")
    (define-values (new-body new-internals new-lifts)
      (gc-definitions linkl-mode body internals lifts first-internal-pos merged-internals
                      #:assume-pure? (garbage-collect-toplevels-enabled)))

    (log-info "Bundling linklet")
    (define bundle (wrap-bundle linkl-mode new-body new-internals new-lifts
                                excluded-module-mpis
                                get-merge-info
                                (let-values ([(base name dir?) (split-path input-file)])
                                  (string->symbol (path->string name)))))

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
