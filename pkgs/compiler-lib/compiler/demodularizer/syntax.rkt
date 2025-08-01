#lang racket/base
(require (only-in '#%kernel [syntax-serialize kernel:syntax-serialize])
         racket/linklet
         syntax/modcollapse
         "path-submod.rkt"
         "linklet.rkt"
         "import.rkt"
         "one-mod.rkt"
         "binding-lookup.rkt")

(provide register-provides-for-syntax
         deserialize-syntax
         serialize-syntax
         build-stx-data-linklet
         build-stx-linklet)

(define (register-provides-for-syntax register! bulk-binding-registry
                                      path/submod
                                      decl
                                      real-decl)
  (register! bulk-binding-registry
             (path/submod->resolved-module-path path/submod)
             (instance-variable-value decl 'self-mpi)
             (instance-variable-value real-decl 'provides)))

(define (deserialize-syntax real-deserialize-instance stx-data-linklet data-instance
                            bulk-binding-registry
                            syntax-shift-module-path-index
                            path submod self-mpi)
  (cond
    [stx-data-linklet
     (define stx-data-instance (instantiate-linklet stx-data-linklet
                                                    (list real-deserialize-instance
                                                          data-instance)))
     (define vec-box (instance-variable-value stx-data-instance '.deserialized-syntax-vector))
     (cond
       [vec-box
        (unless (unbox vec-box)
          (define deserialize-syntax (instance-variable-value stx-data-instance '.deserialize-syntax))
          (deserialize-syntax bulk-binding-registry))
        (define vec (unbox vec-box))
        ;; Shift to an mpi with the actual path so that bulk bindings can be found in modules
        ;; relative to this one, and so we can recognize the full path when re-serializing syntax
        (define stx-mpi (module-path-index-join (if (null? submod) path `(submod ,path ,@submod)) #f))
        (values (for/vector ([stx (in-vector vec)])
                  (syntax-shift-module-path-index stx self-mpi stx-mpi))
                stx-mpi)]
       [else (values #f #f)])]
    [else (values #f #f)]))

(define (serialize-syntax stx-vec self-mpi
                          import-mpis excluded-module-mpis included-module-phases
                          names transformer-names one-mods
                          symbol-module-paths)
  (define (derived-from-self? mpi)
    (define-values (name base) (module-path-index-split mpi))
    (if base
        (and (module-path-index? base)
             (derived-from-self? base))
        (not name)))

  (for ([top-mpi (in-list import-mpis)])
    (let loop ([mpi top-mpi])
      (unless (eq? mpi self-mpi)
        (define-values (name base) (module-path-index-split mpi))
        (if base
            (loop base)
            (unless name
              (error "import MPI is not based on self" top-mpi))))))

  ;; Bindings inside of scopes inside of syntax objects each have a
  ;; module path index (MPI) to specify what the binding refers to.
  ;; That MPI is is relative, though, and the path to get to the MPI
  ;; via syntax objects provides shifts that allow the MPI to be
  ;; turned into a resolved module path. The `report-shift` callback
  ;; that we supply to `kernel:syntax-serialize` lets us build up a
  ;; mapping of those MPIs to fully shifted MPIs, based on the path
  ;; through the syntax object to reach it. Technically, there's no
  ;; guarantee that an MPI will be used in a single resolution, so we
  ;; watch out for that, but it shouldn't happen in practice. (It
  ;; could happen if the same compiled module code is instantiated for
  ;; two different module paths, since the code sharing should be
  ;; detected by caching, and then the same deserialized and cached
  ;; syntax data would be the starting point for each instantiation.)
  (define mpi-map (make-hasheq)) ; mpi -> (cons mpi path/submod)

  (define-values (serialized-stx stx-mpis-vec)
    (cond
      [(= 0 (vector-length stx-vec))
       (values #f (list->vector (cons self-mpi import-mpis)))]
      [else
       (define keep-bulk-module-names (make-hash))
       (for ([(path/submod one-mod) (in-hash one-mods)])
         (when (one-mod-excluded? one-mod)
           (hash-set! keep-bulk-module-names (path/submod->resolved-module-path path/submod) #t)))
       (for ([mod-path (in-hash-keys symbol-module-paths)])
         (hash-set! keep-bulk-module-names (make-resolved-module-path mod-path) #t))
       (kernel:syntax-serialize stx-vec
                                #f ; base-mpi
                                '() ; preserve-prop-keys
                                keep-bulk-module-names
                                #f ; as-data?
                                (cons self-mpi import-mpis) ;; these mpis first, needed for imports
                                ;; report-shift
                                (lambda (mpi shifted-mpi)
                                  #;(log-error "report ~s ~s" (eq-hash-code mpi) mpi)
                                  (when (derived-from-self? shifted-mpi)
                                    (raise-arguments-error 'demodularize
                                                           "a binding's module path index has no resolution in context"
                                                           "binding module path index" mpi
                                                           "in-context module path index" shifted-mpi))
                                  (define p (module-path-index-resolve shifted-mpi))
                                  (define path/submod (resolved-module-path->path/submod p))
                                  (cond
                                    [(hash-ref mpi-map mpi #f)
                                     => (lambda (new-mpi+path/submod)
                                          (unless (equal? path/submod (cdr new-mpi+path/submod))
                                            (raise-arguments-error
                                             'demodularize
                                             "a binding's module path index has different resolution in different contexts"
                                             "resolution" (cdr new-mpi+path/submod)
                                             "other resolution" path/submod)))]
                                    [else
                                     ;; If the result path is to an excluded module, then
                                     ;; we have a replacement mpi to supply the right form
                                     ;; of reference for the excluded module
                                     (define exp-mpi+phase (if (symbol? path/submod)
                                                               (cons (module-path-index-join `(quote ,path/submod) #f) 0)
                                                               ;; Note: don't need to check for `in-phase-level` mapping,
                                                               ;; because that's only for a slice mode that doesn't keep
                                                               ;; syntax objects
                                                               (hash-ref excluded-module-mpis path/submod #f)))
                                     (define new-mpi
                                       (cond
                                         [exp-mpi+phase
                                          (module-path-index-join (collapse-module-path-index (car exp-mpi+phase))
                                                                  self-mpi)]
                                         [else
                                          ;; Otherwise, it must be one we want to refer to this module
                                          self-mpi]))
                                     (hash-set! mpi-map mpi (cons new-mpi path/submod))]))
                                ;; map-mpi
                                (lambda (mpi)
                                  (car (or (hash-ref mpi-map mpi #f)
                                           (raise-arguments-error 'demodularize
                                                                  "found module path index in syntax without reported resolution"
                                                                  "module path index" mpi))))
                                ;; map-binding-symbol
                                (lambda (mpi sym phase)
                                  (define new-mpi+path/submod (hash-ref mpi-map mpi #f))
                                  (unless new-mpi+path/submod
                                    (raise-arguments-error 'demodularize
                                                           "found module path index in syntax binding without reported resolution"
                                                           "module path index" mpi))
                                  (define path/submod (cdr new-mpi+path/submod))
                                  (binding-lookup path/submod phase sym
                                                  names transformer-names
                                                  one-mods
                                                  excluded-module-mpis included-module-phases)))]))

  (for ([stx-mpi (in-vector stx-mpis-vec)]
        [orig-mpi (in-list (cons self-mpi import-mpis))]
        [i (in-naturals)])
    (unless (eq? stx-mpi orig-mpi)
      (error 'syntax-bundle "unexpected MPI for import: ~s versus ~s, index ~a" stx-mpi orig-mpi i)))

  (define all-mpis (vector->list stx-mpis-vec))

  (values all-mpis serialized-stx))

(define (build-stx-data-linklet stx-vec serialized-stx)
  (s-exp->linklet
   'syntax-literals-data
   `(linklet
        ([deserialize-module-path-indexes
          syntax-module-path-index-shift
          syntax-shift-phase-level
          module-use
          deserialize]
         [.mpi-vector])
        (.deserialized-syntax-vector
         .deserialize-syntax)
      (define-values (.deserialized-syntax-vector) (box #f))
      (define-values (.deserialize-syntax)
        (lambda (.bulk-binding-registry)
          (let-values ([(vec) (let-values ([(.inspector) #f])
                                ,serialized-stx)])
            (begin
                (letrec-values ([(loop)
                                 (lambda ()
                                   (if (box-cas! .deserialized-syntax-vector #f vec)
                                       vec
                                       (let-values ([(other-vec) (unbox .deserialized-syntax-vector)])
                                         (if other-vec
                                             other-vec
                                             (loop)))))])
                  (loop))
                (set! .deserialize-syntax #f))))))))

(define (build-stx-linklet stx-vec)
  (s-exp->linklet
   'syntax-literals
   `(linklet
        ([force-syntax-object]
         [.mpi-vector]
         [.deserialized-syntax-vector
          .deserialize-syntax]
         [.namespace
          .phase
          .self
          .inspector
          .bulk-binding-registry
          .set-transformer!])
        (.get-syntax-literal!
         get-encoded-root-expand-ctx)
      (define-values (.syntax-literals)
        (make-vector ,(vector-length stx-vec) #f))
      (define-values (.get-syntax-literal!)
        (lambda (pos)
          (let-values ([(ready-stx) (unsafe-vector*-ref .syntax-literals pos)])
            (if ready-stx
                ready-stx
                (force-syntax-object .syntax-literals
                                     pos
                                     (vector-ref .mpi-vector 0) ; compile-time self
                                     .self ; run-time self
                                     .phase
                                     .inspector
                                     .deserialized-syntax-vector
                                     .bulk-binding-registry
                                     .deserialize-syntax)))))
      (define-values (get-encoded-root-expand-ctx) (quote empty)))))
