#lang racket/base
(require racket/set
         compiler/cm
         racket/file
         racket/path
         compiler/zo-structs
         "module.rkt"
         "pane.rkt"
         "runs.rkt"
         "name.rkt"
         "import-name.rkt"
         "merge.rkt"
         "provide.rkt"
         "simplify.rkt"
         "gc.rkt"
         "bundle.rkt"
         "write.rkt"
         "linklet.rkt"
         "one-mod.rkt"
         "path-submod.rkt"
         "log.rkt")

(provide demodularize

         garbage-collect-toplevels-enabled
         current-excluded-modules
         recompile-enabled
         current-work-directory
         syntax-object-preservation-enabled
         submodule-preservation-enabled
         current-merged-output-file
         current-merged-machine-independent-output-file)

(define current-excluded-modules (make-parameter (set)))
(define garbage-collect-toplevels-enabled (make-parameter #f))
(define recompile-enabled (make-parameter 'auto))
(define current-work-directory (make-parameter #f))
(define syntax-object-preservation-enabled (make-parameter #f))
(define submodule-preservation-enabled (make-parameter #f))
(define current-merged-output-file (make-parameter #f))
(define current-merged-machine-independent-output-file (make-parameter #f))

(define (demodularize given-input-file [given-output-file #f]
                      #:includes [given-includes #f]
                      #:excludes [given-excludes (for/list ([path (in-set (current-excluded-modules))])
                                                   (list 'module path))]
                      #:keep-syntax? [keep-syntax? (syntax-object-preservation-enabled)]
                      #:include-submodules [include-submods (if keep-syntax?
                                                                #f
                                                                '((main) (configure-runtime)))]
                      #:exclude-submodules [exclude-submods '()]
                      #:work-directory [given-work-directory (current-work-directory)]
                      #:prune-definitions? [prune-definitions? (garbage-collect-toplevels-enabled)]
                      #:recompile [recompile-mode (recompile-enabled)]
                      #:return-bundle? [return-bundle? #f]
                      #:dump-output-file [dump-output-file (current-merged-output-file)]
                      #:dump-mi-output-file [dump-linklet-file (current-merged-machine-independent-output-file)]
                      #:keep-submodules? [keep-submodules? (submodule-preservation-enabled)]
                      #:external-singetons? [external-singletons? keep-syntax?])
  (define input-path (simple-form-path given-input-file))

  (define work-directory (or given-work-directory
                             (make-temporary-file "demod-work-~a" 'directory)))

  (log-demodularizer-info (format "Compiling modules to ~s" work-directory))
  (parameterize ([current-namespace (make-empty-namespace)]
                 [current-compiled-file-roots (list (build-path work-directory "native")
                                                    (build-path work-directory "linklet"))]
                 [current-compile-target-machine #f]
                 [current-multi-compile-any #t])
    (namespace-attach-module (variable-reference->namespace (#%variable-reference)) ''#%builtin)
    (managed-compile-zo input-path))

  (log-demodularizer-info "Finding modules")
  (define-values (all-one-mods submods common-excluded-module-mpis symbol-module-paths)
    (parameterize ([current-compiled-file-roots (list (build-path work-directory "linklet"))])
      (find-modules input-path
                    #:includes given-includes
                    #:excludes given-excludes
                    #:include-submods include-submods
                    #:exclude-submods exclude-submods
                    #:keep-syntax? keep-syntax?)))

  (when (and work-directory (not given-work-directory))
    (delete-directory/files work-directory))

  (log-demodularizer-info "Partitioning modules")
  (define-values (all-sorted-panes added-pane-submods)
    (partition-panes all-one-mods input-path submods
                     #:slice? (not keep-syntax?)
                     #:external-singetons? external-singletons?
                     #:include-submods include-submods
                     #:exclude-submods exclude-submods))
  (define-values (top-path/submods excluded-module-mpiss included-module-phasess one-mods)
    (reify-panes all-sorted-panes all-one-mods common-excluded-module-mpis
                 #:slice? (not keep-syntax?)))
  (log-demodularizer-info " introduced partitions: ~a" (length added-pane-submods))

  (log-demodularizer-info "Finding module bodies to merge")
  (define-values (phase-runss excluded-modules-to-requires)
    (for/lists (phase-runss excluded-modules-to-requires)
        ([top-path/submod (in-list top-path/submods)]
         [excluded-module-mpis (in-list excluded-module-mpiss)])
      (find-runs top-path/submod
                 one-mods
                 excluded-module-mpis
                 #:max-phase (and (not keep-syntax?)
                                  0))))

  (log-demodularizer-info "Selecting names")
  (define-values (names transformer-names internals find-or-add-name!)
    (select-names one-mods
                  phase-runss))
  (define new-phase-runss
    (for/list ([phase-runs (in-list phase-runss)]
               [excluded-module-mpis (in-list excluded-module-mpiss)])
      (add-import-maps phase-runs find-or-add-name! names ; <--- `names` is modified to add new names
                       one-mods excluded-module-mpis)))

  (log-demodularizer-info "Merging linklets")
  (define-values (phase-mergeds name-importss stx-vecs portal-stxess)
    (for/lists (phase-mergeds name-importss stx-vecs portal-stxess)
        ([phase-runs (in-list new-phase-runss)])
      (merge-linklets phase-runs names transformer-names
                      #:prune-definitions? prune-definitions?)))
  (define provided-namess ; (list (root-phase -> (list sym ...)))
    (for/list ([top-path/submod (in-list top-path/submods)]
               [excluded-module-mpis (in-list excluded-module-mpiss)]
               [included-module-phases (in-list included-module-phasess)])
      (provides-to-names (one-mod-provides (hash-ref one-mods top-path/submod))
                         names transformer-names
                         one-mods
                         excluded-module-mpis included-module-phases
                         #:keep-syntax? keep-syntax?)))

  (log-demodularizer-info "Simplifying merged linklet")
  (define simplified-phase-mergeds
    (for/list ([phase-merged (in-list phase-mergeds)])
      (simplify-linklet phase-merged)))

  ;; Connects GC to needed exports in bundle
  (define used-externally (make-hasheqv)) ; symbol -> #t

  (define new-phase-mergeds
    (cond
      [(and keep-syntax?
            (not prune-definitions?))
       ;; any definition might be referenced reflectively
       simplified-phase-mergeds]
      [else
       (log-demodularizer-info "GCing definitions")
       (define used (make-hasheq)) ; symbol -> 'used or thunk
       (for ([phase-merged (in-list simplified-phase-mergeds)]
             [provided-names (in-list provided-namess)]
             [stx-vec (in-list stx-vecs)]
             [excluded-module-mpis (in-list excluded-module-mpiss)]
             [included-module-phases (in-list included-module-phasess)])
         (gc-find-uses! used used-externally
                        phase-merged
                        provided-names
                        stx-vec
                        names transformer-names
                        one-mods
                        excluded-module-mpis included-module-phases
                        #:keep-defines? (and keep-syntax?
                                             (not prune-definitions?))
                        #:prune-definitions? prune-definitions?))
       (for/list ([phase-merged (in-list simplified-phase-mergeds)])
         (gc-definitions used phase-merged))]))

  (log-demodularizer-info "Bundling linklet")
  (when (and dump-output-file (file-exists? dump-output-file))
    (delete-file dump-output-file))
  (define dir-ht
    (for/hash ([top-path/submod (in-list top-path/submods)]
               [phase-merged (in-list new-phase-mergeds)]
               [name-imports (in-list name-importss)]
               [stx-vec (in-list stx-vecs)]
               [portal-stxes (in-list portal-stxess)]
               [excluded-modules-to-require (in-list excluded-modules-to-requires)]
               [excluded-module-mpis (in-list excluded-module-mpiss)]
               [included-module-phases (in-list included-module-phasess)])
      (define m (hash-ref one-mods top-path/submod))
      (define path (path/submod-path top-path/submod))
      (define submod (path/submod-submod top-path/submod))
      (define file-name
        (let-values ([(base name dir?) (split-path path)])
          (string->symbol (path->string (path-replace-extension name #"")))))
      (define module-name (if (pair? submod)
                              (cons file-name submod)
                              file-name))
      (define (only-kept-submodules submod-syms)
        (for/list ([submod-sym (in-list submod-syms)]
                   #:do [(define sub-submod (append submod (list submod-sym)))]
                   #:when (and (or (not include-submods)
                                   (member sub-submod include-submods))
                               (not (member sub-submod exclude-submods))))
          submod-sym))
      (define bundle
        (wrap-bundle module-name phase-merged name-imports
                     stx-vec portal-stxes
                     excluded-modules-to-require excluded-module-mpis included-module-phases
                     (one-mod-provides m)
                     names transformer-names one-mods
                     symbol-module-paths
                     #:import/export-only (and (or (not keep-syntax?)
                                                   prune-definitions?)
                                               used-externally)
                     #:pre-submodules (append
                                       (if (null? submod)
                                           added-pane-submods
                                           null)
                                       (only-kept-submodules
                                        (one-mod-pre-submodules m)))
                     #:post-submodules (only-kept-submodules
                                        (one-mod-post-submodules m))
                     #:dump-output-file dump-output-file))
      (values submod bundle)))

  (define bundle
    (if (= 1 (hash-count dir-ht))
        (hash-ref dir-ht '())
        (linkl-directory dir-ht)))

  (cond
    [return-bundle?
     (log-demodularizer-info "Writing bytecode")
     (define o (open-output-bytes))
     (write-module o bundle)
     (parameterize ([read-accept-compiled #t])
       (read (open-input-bytes (get-output-bytes o))))]
    [else
     (log-demodularizer-info "Writing bytecode")
     (define recompile? (or (eq? (recompile-enabled) #t)
                            (eq? (recompile-enabled) 'auto)))
     (define output-file (or given-output-file
                             (path-add-suffix input-path #"_merged.zo")))
     (define intermediate-file (or (and recompile?
                                        dump-linklet-file)
                                   output-file))
     (write-module intermediate-file bundle)

     (cond
       [recompile?
        (log-demodularizer-info "Recompiling and rewriting bytecode")
        (define zo (compiled-expression-recompile
                    (parameterize ([read-accept-compiled #t])
                      (call-with-input-file* intermediate-file read))))
        (call-with-output-file* output-file
                                #:exists 'replace
                                (lambda (out) (write zo out)))]
       [dump-linklet-file
        (copy-file intermediate-file dump-linklet-file)])]))
