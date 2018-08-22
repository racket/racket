#lang racket/base
(require racket/set
         compiler/cm
         "find.rkt"
         "name.rkt"
         "merge.rkt"
         "gc.rkt"
         "bundle.rkt"
         "write.rkt")

(provide demodularize

         garbage-collect-toplevels-enabled
         current-excluded-modules
         recompile-enabled)

(define garbage-collect-toplevels-enabled (make-parameter #f))
(define recompile-enabled (make-parameter #f))

(define logger (make-logger 'demodularizer (current-logger)))

(define (demodularize input-file [given-output-file #f])
  (parameterize ([current-logger logger]
                 [current-excluded-modules (for/set ([path (in-set (current-excluded-modules))])
                                             (normal-case-path (simplify-path (path->complete-path path))))])

    (log-info "Compiling module")
    (parameterize ([current-namespace (make-base-empty-namespace)])
      (managed-compile-zo input-file))

    (log-info "Finding modules")
    (define-values (runs excluded-module-mpis) (find-modules input-file))

    (log-info "Selecting names")
    (define-values (names internals lifts imports) (select-names runs))

    (log-info "Merging linklets")
    (define-values (body first-internal-pos get-merge-info)
      (merge-linklets runs names internals lifts imports))

    (log-info "GCing definitions")
    (define-values (new-body new-internals new-lifts)
      (gc-definitions body internals lifts first-internal-pos
                      #:assume-pure? (garbage-collect-toplevels-enabled)))

    (log-info "Bundling linklet")
    (define bundle (wrap-bundle new-body new-internals new-lifts
                                excluded-module-mpis
                                get-merge-info
                                (let-values ([(base name dir?) (split-path input-file)])
                                  (string->symbol (path->string name)))))

    (log-info "Writing bytecode")
    (define output-file (or given-output-file
                            (path-add-suffix input-file #"_merged.zo")))
    (write-module output-file bundle)

    (when (recompile-enabled)
      (log-info "Recompiling and rewriting bytecode")
      (define zo (compiled-expression-recompile
                  (parameterize ([read-accept-compiled #t])
                    (call-with-input-file* output-file read))))
      (call-with-output-file* output-file
                              #:exists 'replace
                              (lambda (out) (write zo out))))))
