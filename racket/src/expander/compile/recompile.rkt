#lang racket/base
(require "../host/linklet.rkt"
         "../eval/reflect.rkt"
         "../compile/form.rkt"
         "../compile/module.rkt"
         "../namespace/namespace.rkt"
         "../namespace/module.rkt"
         "../common/module-path.rkt"
         "linklet.rkt"
         "correlated-linklet.rkt"
         "form.rkt"
         "serialize.rkt"
         "reserved-symbol.rkt"
         "instance.rkt"
         "extra-inspector.rkt"
         "compiled-in-memory.rkt")

(provide compiled-expression-recompile)

(define (compiled-expression-recompile c)
  (unless (compiled-expression? c)
    (raise-argument-error 'compiled-expression-recompile "compiled-expression?" c))
  (define target-machine (current-compile-target-machine))
  (cond
    [(not target-machine)
     ;; There's no use for machine-independent mode, and
     ;; `recompile-bundle` assumes that it should actually compile
     c]
    [(or (linklet-bundle? c)
         (linklet-directory? c))
     (define ns (current-namespace))
     ;; First, extract all bundles, so we can implement cross-module
     ;; optimizations involving submodules
     (define bundles (extract-linklet-bundles c '() #hash()))
     ;; Recompile each bundle
     (define recompileds (make-hash))
     (define (force-recompile-bundle k)
       (unless (hash-ref recompileds k #f)
         (hash-set! recompileds k 'in-process)
         (define b (hash-ref bundles k #f))
         (unless b
           (raise-arguments-error 'compiled-expression-recompile
                                  "cannot find submodule"
                                  "submodule path" k))
         (hash-set! recompileds k (recompile-bundle b
                                                    force-recompile-bundle
                                                    ns
                                                    target-machine)))
       (hash-ref recompileds k))
     (for ([k (in-hash-keys bundles)])
       (force-recompile-bundle k))
     (replace-linklet-bundles c '() recompileds)]
    [else
     ;; For now, we just give up on compiled-in-memory information,
     ;; because the intended use cases are with serialization --- but
     ;; beware that we may lose detailed inspector-based access:
     (compiled-expression-recompile (compiled-in-memory-linklet-directory c))]))

;; ----------------------------------------

(define (extract-linklet-bundles c rev-path accum)
  (cond
    [(linklet-bundle? c)
     (hash-set accum (reverse rev-path) c)]
    [(linklet-directory? c)
     (for/fold ([accum accum]) ([(k v) (in-hash (linklet-directory->hash c))])
       (cond
         [(symbol? k)
          (extract-linklet-bundles v (cons k rev-path) accum)]
         [(not k)
          (extract-linklet-bundles v rev-path accum)]
         [else accum]))]
    [else accum]))

(define (replace-linklet-bundles c rev-path recompileds)
  (cond
    [(linklet-bundle? c)
     (recompiled-bundle (hash-ref recompileds (reverse rev-path)))]
    [(linklet-directory? c)
     (hash->linklet-directory
      (for/hasheq ([(k v) (in-hash (linklet-directory->hash c))])
        (values k
                (cond
                  [(symbol? k)
                   (replace-linklet-bundles v (cons k rev-path) recompileds)]
                  [(not k)
                   (replace-linklet-bundles v rev-path recompileds)]))))]
    [else c]))

;; ----------------------------------------

(struct recompiled (bundle
                    ;; The remaining information is used for cross-linklet
                    ;; inlnining among submodules within a linklet directory
                    phase-to-link-module-uses
                    self)
  #:authentic)

;; Takes a bundle and returns a recompiled
(define (recompile-bundle b get-submodule-recompiled ns target-machine)
  ;; We have to execute the parts of the bundle that supply data, such
  ;; as the mpis and link modules, then use that data for cross-module
  ;; optimization while recompiling the per-phase body units, and then
  ;; regenerate the data linklets because optimization can add new
  ;; linklet import.s
  (define orig-h (linklet-bundle->hash b))

  ;; Force compilation of linklets that are not the module body:
  (define h (for/hasheq ([(k v) (in-hash orig-h)])
              (cond
                [(and (not (exact-integer? k))
                      (correlated-linklet? v))
                 (values k (force-compile-linklet v))]
                [else (values k v)])))

  ;; For now, there's ony one target machine that is supported by each VM:
  (define can-eval-compiled?
    (eq? target-machine (system-type 'target-machine)))

  (define (eval-metadata-linklet key)
    (if can-eval-compiled?
        (eval-linklet (hash-ref h key))
        (eval-correlated-linklet (hash-ref orig-h key))))

  (define data-instance
    (instantiate-linklet (eval-metadata-linklet 'data)
                         (list deserialize-instance)))
  (define declaration-instance
    (instantiate-linklet (eval-metadata-linklet 'decl)
                         (list deserialize-instance
                               data-instance)))
  (define (decl key)
    (instance-variable-value declaration-instance key))

  (define mpis (make-module-path-index-table))
  ;; Add current mpis in order, so existing references will stay correct
  (for ([mpi (in-vector (instance-variable-value data-instance mpi-vector-id))])
    (add-module-path-index! mpis mpi))

  (define self (decl 'self-mpi))
  (define phase-to-link-modules (decl 'phase-to-link-modules))

  (define (find-submodule mod-name phase)
    ;; If `mod-name` refers to a submodule in the same linklet directory,
    ;; then we need to force that one to be recompiled and then return it.
    (define find-l (resolved-module-path-name mod-name))
    (define self-l (resolved-module-path-name (module-path-index-resolve self)))
    (define (root-of l) (if (pair? l) (car l) l))
    (cond
      [(equal? (root-of find-l) (root-of self-l))
       (define r (get-submodule-recompiled (if (pair? find-l) (cdr find-l) '())))
       (when (eq? r 'in-process)
         (raise-arguments-error 'compiled-expression-recompile
                                "cycle in linklet imports"))
       (define b (recompiled-bundle r))
       (define linklet
         (or (hash-ref (linklet-bundle->hash b) phase #f)
             (raise-arguments-error 'compiled-expression-recompile
                                    "cannot find submodule at phase"
                                    "submodule" mod-name
                                    "phase" phase)))
       (module-linklet-info linklet
                            (hash-ref (recompiled-phase-to-link-module-uses r) phase #f)
                            (recompiled-self r)
                            #f ; inspector is the same as the module being compiled
                            (current-code-inspector) ; compile-time inspector is now
                            #f)]
      [else #f]))

  (define body-linklets+module-use*s
    (for/hash ([(phase body-linklet) (in-hash h)]
               #:when (exact-integer? phase))
      (define module-use*s
        (module-uses-add-extra-inspectorsss (hash-ref phase-to-link-modules phase)
                                            #f))
      (define-values (linklet new-module-use*s)
        (compile-module-linklet (if (correlated-linklet? body-linklet)
                                    (correlated-linklet-expr body-linklet)
                                    body-linklet)
                                #:compile-linklet (if (correlated-linklet? body-linklet)
                                                      compile-linklet
                                                      recompile-linklet)
                                #:body-imports `([,get-syntax-literal!-id]
                                                 [,set-transformer!-id])
                                #:body-import-instances (list empty-syntax-literals-instance
                                                              empty-module-body-instance)
                                #:get-module-linklet-info find-submodule
                                #:serializable? #t
                                #:module-prompt? #t
                                #:module-use*s module-use*s
                                #:cross-linklet-inlining? #t
                                #:load-modules? #t
                                #:namespace ns))
      (values phase (cons linklet new-module-use*s))))

  (define h/new-body-linklets
    (for/fold ([h h]) ([(phase l+mu*s) (in-hash body-linklets+module-use*s)])
      (hash-set h phase (car l+mu*s))))

  (define phase-to-link-module-uses
    (for/hasheq ([(phase l+mu*s) (in-hash body-linklets+module-use*s)])
      (values phase (module-uses-strip-extra-inspectorsss (cdr l+mu*s)))))
  
  (define phase-to-link-module-uses-expr
    (serialize-phase-to-link-module-uses phase-to-link-module-uses mpis))

  (define data-linklet
    (compile-linklet (generate-module-data-linklet mpis) 'data))

  (define declaration-linklet
    (compile-linklet (generate-module-declaration-linklet mpis self 
                                                          (decl 'requires)
                                                          (decl 'provides)
                                                          phase-to-link-module-uses-expr)
                     'decl))

  (define new-bundle
    (hash->linklet-bundle (let* ([h h/new-body-linklets]
                                 [h (hash-set h 'data data-linklet)]
                                 [h (hash-set h 'decl declaration-linklet)])
                            h)))

  (recompiled new-bundle
              phase-to-link-module-uses
              self))
