#lang racket/base
(require (only-in '#%kernel [syntax-serialize kernel:syntax-serialize])
         racket/linklet
         syntax/modcollapse
         "linklet.rkt")

(provide register-provides-for-syntax
         deserialize-syntax
         serialize-syntax
         build-stx-data-linklet
         build-stx-linklet)

(define (register-provides-for-syntax register! bulk-binding-registry
                                      orig-path submod
                                      decl
                                      real-decl)
  (register! bulk-binding-registry
             (make-resolved-module-path (let ([p (if (string? orig-path)
                                                     (string->path orig-path)
                                                     orig-path)])
                                          (if (pair? submod) (cons p submod) p)))
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
     (define vec (instance-variable-value stx-data-instance '.deserialized-syntax-vector))
     (cond
       [vec
        (unless (vector-ref vec 0)
          (define deserialize-syntax (instance-variable-value stx-data-instance '.deserialize-syntax))
          (deserialize-syntax bulk-binding-registry))
        ;; Shift to an mpi with the actual path so that bulk bindings can be found in modules
        ;; relative to this one, and so we can recognize the full path when re-serializing syntax
        (define stx-mpi (module-path-index-join (if (null? submod) path `(submod ,path ,@submod)) #f))
        (values (for/vector ([stx (in-vector vec)])
                  (syntax-shift-module-path-index stx self-mpi stx-mpi))
                stx-mpi)]
       [else (values #f #f)])]
    [else (values #f #f)]))

(define (serialize-syntax stx-vec import-paths excluded-module-mpis names)
  (define self-mpi (module-path-index-join #f #f))

  (define import-mpis
    (for/list ([path (in-list import-paths)])
      (cond
        [(symbol? path) (module-path-index-join `(quote ,path) #f)]
        [else
         ;; collapse to a simplified MPI early
         (define mpi (or (hash-ref excluded-module-mpis path #f)
                         (error 'import-mpis "cannot find module: ~s" path)))
         (module-path-index-join (collapse-module-path-index mpi)
                                 ;; keep the "self" mpi, if any:
                                 (let loop ([mpi mpi])
                                   (define-values (name base) (module-path-index-split mpi))
                                   (if (not name)
                                       mpi
                                       (and (module-path-index? base) (loop base)))))])))

  (define (derived-from-self? mpi)
    (define-values (name base) (module-path-index-split mpi))
    (if base
        (and (module-path-index? base)
             (derived-from-self? base))
        (not name)))

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
       (kernel:syntax-serialize stx-vec
                                #f ; base-mpi
                                '() ; preserve-prop-keys
                                #f ; provides-namespace
                                #f ; as-data?
                                (cons self-mpi import-mpis) ;; these mpis first, needed for imports
                                ;; report-shift
                                (lambda (mpi shifted-mpi)
                                  #;(log-error "report ~s ~s" (eq-hash-code mpi) mpi)
                                  (when (derived-from-self? shifted-mpi)
                                    (raise-arguments-error 'demodularize
                                                           "a binding's mpdule path index has no resolution in context"
                                                           "binding module path index" mpi
                                                           "in-context module path index" shifted-mpi))
                                  (define p (module-path-index-resolve shifted-mpi))
                                  (define path/submod (resolved-module-path-name p))
                                  (cond
                                    [(hash-ref mpi-map mpi #f)
                                     => (lambda (new-mpi+path/submod)
                                          (unless (equal? path/submod (cdr new-mpi+path/submod))
                                            (raise-arguments-error
                                             'demodularize
                                             "a binding's mpdule path index has different resolution in different contexts"
                                             "resolution" (cdr new-mpi+path/submod)
                                             "other resolution" path/submod)))]
                                    [else
                                     ;; If the result path is to an excluded module, then
                                     ;; we have a replacement mpi to supply the right form
                                     ;; of reference for the excluded module
                                     (define exp-mpi (if (symbol? path/submod)
                                                         (module-path-index-join `(quote ,path/submod) #f)
                                                         (hash-ref excluded-module-mpis path/submod #f)))
                                     (define new-mpi
                                       (cond
                                         [exp-mpi
                                          (module-path-index-join (collapse-module-path-index exp-mpi)
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
                                (lambda (mpi phase sym)
                                  (define new-mpi+path/submod (hash-ref mpi-map mpi #f))
                                  (unless new-mpi+path/submod
                                    (raise-arguments-error 'demodularize
                                                           "found module path index in syntax binding without reported resolution"
                                                           "module path index" mpi))
                                  (define path/submod (cdr new-mpi+path/submod))
                                  (cond
                                    [(hash-ref excluded-module-mpis path/submod #f)
                                     sym]
                                    [(symbol? path/submod)
                                     sym]
                                    [else
                                     (or (hash-ref names (cons (cons path/submod phase) sym) #f)
                                         (raise-arguments-error 'demodularize
                                                                "did not find new name for binding in syntax"
                                                                "module path" path/submod
                                                                "name" sym
                                                                "phase level" phase))])))]))

  (for ([stx-mpi (in-vector stx-mpis-vec)]
        [orig-mpi (in-list (cons self-mpi import-mpis))])
    (unless (eq? stx-mpi orig-mpi)
      (error "unexpected MPI for import")))

  (define all-mpis (vector->list stx-mpis-vec))

  (values self-mpi all-mpis serialized-stx))

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
      (define-values (.deserialized-syntax-vector)
        (make-vector ,(vector-length stx-vec) #f))
      (define-values (.deserialize-syntax)
        (lambda (.bulk-binding-registry)
          (begin
            (vector-copy! .deserialized-syntax-vector
                          '0
                          (let-values ([(.inspector) #f])
                            ,serialized-stx))
            (set! .deserialize-syntax #f)))))))

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
