#lang racket/base
(require (only-in '#%linklet primitive->compiled-position)
         racket/set
         compiler/zo-structs
         racket/pretty
         "run.rkt"
         "name.rkt"
         "linklet.rkt"
         "syntax.rkt"
         "binding.rkt"
         "deshadow.rkt")

(provide wrap-bundle
         current-merged-output-file)

(define current-merged-output-file (make-parameter #f))

(define (wrap-bundle linkl-mode phase-body phase-internals phase-lifts phase-import-keys
                     portal-stxes phase-defined-names
                     excluded-modules-to-require excluded-module-mpis provides names
                     get-merge-info name
                     #:export? export?)
  (define-values (phase-runs
                  phase-more
                  stx-vec)
    (get-merge-info))
  (define-values (min-phase max-phase)
    (for/fold ([min-phase 0] [max-phase 0]) ([phase (in-hash-keys phase-body)])
      (values (min phase min-phase) (max phase max-phase))))

  ;; Gather all paths that are either leftover imports to a linklet,
  ;; required overall as excluded modules, or mentioned in a provided
  ;; binding. Leftover imports and provides should be transitively
  ;; required by the excluded modules, but we don't try to check that.
  (define-values (external-path-pos external-paths)
    (let ()
      (define (add-path path/submod ht rev-paths)
        (if (hash-ref ht path/submod #f)
            (values ht rev-paths)
            (values (hash-set ht path/submod (add1 (hash-count ht)))
                    (cons path/submod rev-paths))))
      (define-values (import-ht import-rev-paths)
        (for*/fold ([ht #hash()] [rev-paths '()])
                   ([import-keys (in-hash-values phase-import-keys)]
                    [path/submod+phase (in-list import-keys)])
          (define path/submod (car path/submod+phase))
          (add-path path/submod ht rev-paths)))
      (define-values (require-ht require-rev-paths)
        (for*/fold ([ht import-ht] [rev-paths import-rev-paths])
                   ([path/submod+phase (in-list excluded-modules-to-require)])
          (define path/submod (car path/submod+phase))
          (add-path path/submod ht rev-paths)))
      (define-values (provide-ht provide-rev-paths)
        (for*/fold ([ht require-ht] [rev-paths require-rev-paths])
                   ([binds (in-hash-values provides)]
                    [bind (in-hash-values binds)]
                    [mpi (in-list (binding-mpis bind))]
                    #:do [(define r (module-path-index-resolve mpi))
                          (define path/submod (resolved-module-path-name r))]
                    #:when (or (symbol? path/submod)
                               (hash-ref excluded-module-mpis path/submod #f)))
          (add-path path/submod ht rev-paths)))
      (values provide-ht (reverse provide-rev-paths))))

  (define-values (self-mpi all-mpis serialized-stx)
    (serialize-syntax stx-vec external-paths excluded-module-mpis names))

  (define module-name 'demodularized)

  (define serialized-mpis
    ;; Construct two vectors: one for mpi construction, and
    ;; another for selecting the slots that are externally referenced
    ;; mpis (where the selection vector matches the `external-paths` order
    ;; followed by `stx-mpis` in order).
    ;; If all module paths refer to symbol-named primitive modules, then 
    ;; we just make a vector with those specs in order, but if there's a
    ;; more complex mpi, then we have to insert extra slots in the first
    ;; vector to hold intermediate mpi constructions.
    ;; We could do better here by sharing common tails.
    (let loop ([external-paths external-paths]
               [all-mpis (cdr all-mpis)] ; cdr skips self mpi
               [specs (list (box module-name))] ; initial spec = self mpi
               [results (list 0)])              ; initial 0 = self mpi
      (define (mpi-loop mpi specs)
        (define-values (name base) (module-path-index-split mpi))
        (cond
          [(and (not name) (not base))
           (values 0 specs)]
          [(not base)
           (values (length specs) (cons (vector name) specs))]
          [else
           (define-values (next-i next-specs) (mpi-loop base specs))
           (values (length next-specs) (cons (vector name next-i) next-specs))]))
      (cond
        [(null? external-paths)
         (let loop ([stx-mpis all-mpis]
                    [specs specs]
                    [results results])
           (cond
             [(null? stx-mpis)
              (list (list->vector (reverse specs))
                    (list->vector (reverse results)))]
             [else
              (define-values (i new-specs) (mpi-loop (car stx-mpis) specs))
              (loop (cdr stx-mpis) new-specs (cons i results))]))]
        [else
         (define path (car external-paths))
         (cond
           [(or (symbol? path) (and (pair? path) (symbol? (car path))))
            (loop (cdr external-paths)
                  (cdr all-mpis)
                  (cons (vector `(quote ,path)) specs)
                  (cons (length specs) results))]
           [(or (path? path) (and (pair? path) (path? (car path))))
            (define-values (i new-specs) (mpi-loop (car all-mpis) specs))
            (loop (cdr external-paths)
                  (cdr all-mpis)
                  new-specs
                  (cons i results))]
           [else
            (error 'wrap-bundle "unrecognized import path shape: ~s" path)])])))

  (define data-linkl
    (case linkl-mode
      [(linkl)
       (linkl 'data
              '((deserialize-module-path-indexes))
              '((#f))
              '(.mpi-vector)
              '()
              '()
              #hasheq()
              (list
               (def-values (list (toplevel 0 2 #f #f)) ; .mpi-vector
                 (application (toplevel 2 1 #f #f) ; deserialize-module-path-indexes
                              serialized-mpis)))
              16
              #f)]
      [(s-exp)
       (s-exp->linklet
        'data
        `(linklet ((deserialize-module-path-indexes))
             (.mpi-vector)
           (define-values (.mpi-vector)
             (deserialize-module-path-indexes (quote ,(car serialized-mpis))
                                              (quote ,(cadr serialized-mpis))))))]))

  (define sorted-phases
    (sort (set->list
           (for/set ([path/submod+phase (in-list excluded-modules-to-require)])
             (cdr path/submod+phase)))
          <))

  (define serialized-requires
    (list->vector
     (let loop ([phases sorted-phases])
       (cond
         [(null? phases) (list '())]
         [else
          (define phase (car phases))
          (define n (for/sum ([path/submod+phase (in-list excluded-modules-to-require)])
                      (if (eqv? phase (cdr path/submod+phase)) 1 0)))
          (append `(#:cons #:list ,(add1 n) ,(- 0 phase))
                  (apply
                   append
                   (for/list ([path/submod+phase (in-list excluded-modules-to-require)]
                              #:when (eqv? phase (cdr path/submod+phase)))
                     (define path/submod (car path/submod+phase))
                     (define pos (hash-ref external-path-pos path/submod))
                     `(#:mpi ,pos)))
                  (loop (cdr phases)))]))))

  (define recur-requires
    (for/list ([phase (in-list sorted-phases)])
      (for/list ([path/submod+phase (in-list excluded-modules-to-require)]
                 #:when (eqv? phase (cdr path/submod+phase)))
        #t)))

  (define serialized-provides
    (let ([phases (hash-keys provides)]) ; deterministic output would need sorting here
      (list->vector
       `(#:hasheqv ,(hash-count provides)
         ,@(apply
            append
            (for/list ([phase (in-list phases)])
              (define ht (hash-ref provides phase))
              `(,phase
                #:hasheq
                ,(hash-count ht)
                ,@(apply
                   append
                   (for/list ([(name bind) (in-hash ht)])
                     `(,name ,@(serialize-binding bind external-path-pos excluded-module-mpis names)))))))))))

  (define (primitive v)
    (primval (or (primitive->compiled-position v)
                 (error "cannot find primitive" v))))

  (define (make-phase-to-link-modules make-apply
                                      get-prim
                                      get-module-use
                                      get-mpi-vector)
    (let ([depth 2])
      (make-apply (get-prim 'hasheqv hasheqv)
                  (apply
                   append
                   (for/list ([(root-phase import-keys) (in-hash phase-import-keys)])
                     (list root-phase
                           (let ([depth (+ depth (length import-keys))])
                             (make-apply (get-prim 'list list)
                                         (for/list ([path/submod+phase (in-list import-keys)])
                                           (define path/submod (car path/submod+phase))
                                           (define pos (hash-ref external-path-pos path/submod))
                                           (let ([depth (+ depth 2)])
                                             (make-apply (get-module-use depth)
                                                         (list
                                                          (let ([depth (+ depth 2)])
                                                            (make-apply (get-prim 'vector-ref vector-ref)
                                                                        (list
                                                                         (get-mpi-vector depth)
                                                                         pos)))
                                                          (cdr path/submod+phase)))))))))))))

  (define decl-linkl
    (case linkl-mode
      [(linkl)
       (let ([deserialize-pos 1]
             [module-use-pos 2]
             [mpi-vector-pos 3]
             [exports-pos 4])
         (linkl 'decl
                '((deserialize
                   module-use)
                  (.mpi-vector))
                '((#f)
                  (#f))
                '(self-mpi requires recur-requires flattened-requires provides phase-to-link-modules portal-stxes)
                '()
                '()
                #hasheq()
                (list
                 (def-values (list (toplevel 0 (+ exports-pos 0) #f #f)) ; .self-mpi
                   (application (primitive vector-ref)
                                (list (toplevel 2 mpi-vector-pos #f #f)
                                      '0)))
                 (def-values (list (toplevel 0 (+ exports-pos 1) #f #f)) ; requires
                   (let ([arg-count 9])
                     (application (toplevel arg-count deserialize-pos #f #f)
                                  (list
                                   (toplevel arg-count mpi-vector-pos #f #f)
                                   #f #f 0 '#() 0 '#() '#()
                                   serialized-requires))))
                 (def-values (list (toplevel 0 (+ exports-pos 2) #f #f)) ; recur-requires
                   recur-requires)
                 (def-values (list (toplevel 0 (+ exports-pos 3) #f #f)) ; flattened-requires
                   #f)
                 (def-values (list (toplevel 0 (+ exports-pos 4) #f #f)) ; provides
                   (application (primitive hasheqv) null))
                 (def-values (list (toplevel 0 (+ exports-pos 5) #f #f)) ; phase-to-link-modules
                   (make-phase-to-link-modules application
                                               (lambda (name prim) (primitive prim))
                                               (lambda (depth) (toplevel depth module-use-pos #f #f))
                                               (lambda (depth) (toplevel depth mpi-vector-pos #f #f))))
                 (def-values (list (toplevel 0 (+ exports-pos 6) #f #f)) ; portal-stxes
                   (application (primitive hasheqv) null)))
                (+ 32 (for/fold ([len 0]) ([import-keys (in-hash-values phase-import-keys)])
                        (max len (length import-keys))))
                #f))]
      [(s-exp)
       (s-exp->linklet
        'decl
        `(linklet ((deserialize
                    module-use)
                   (.mpi-vector))
             (self-mpi requires recur-requires flattened-requires provides phase-to-link-modules portal-stxes)
           (define-values (self-mpi) (vector-ref .mpi-vector 0))
           (define-values (requires) (deserialize .mpi-vector #f #f 0 '#() 0 '#() '#()
                                                  (quote ,serialized-requires)))
           (define-values (recur-requires) (quote ,recur-requires))
           (define-values (flattened-requires) #false)
           (define-values (provides) ,(if (= 0 (hash-count provides))
                                          (quote '#hasheqv())
                                          `(deserialize .mpi-vector #f #f 0 '#() 0 '#() '#()
                                                        (quote ,serialized-provides))))
           (define-values (phase-to-link-modules)
             ,(make-phase-to-link-modules cons
                                          (lambda (name prim) name)
                                          (lambda (depth) 'module-use)
                                          (lambda (depth) '.mpi-vector)))
           (define-values (portal-stxes) '#hasheqv())))]))

  (define body-linkl-ht
    (for/hasheqv ([(root-phase body) (in-hash phase-body)])
      (define internals (hash-ref phase-internals root-phase))
      (define lifts (hash-ref phase-lifts root-phase))
      (define import-keys (hash-ref phase-import-keys root-phase))
      (define runs (hash-ref phase-runs root-phase))

      (define-values (ordered-importss
                      import-shapess
                      any-syntax-literals?
                      any-transformer-registers?
                      saw-zero-pos-toplevel?)
        (apply values (hash-ref phase-more root-phase)))

      (define new-linkl
        (case linkl-mode
          [(linkl)
           (linkl module-name
                  (list* (if any-syntax-literals? '(.get-syntax-literal!) '())
                         (if any-transformer-registers? '(.set-transformer!) '())
                         (for/list ([imports (in-list ordered-importss)])
                           (for/list ([import (in-list imports)])
                             (car import))))
                  (list* (if any-syntax-literals? (list (function-shape 1 #f)) '())
                         (if any-transformer-registers? (list (function-shape 2 #f)) '())
                         import-shapess)
                  '() ; exports
                  internals
                  lifts
                  #hasheq()
                  body
                  (for/fold ([m 0]) ([r (in-list runs)])
                    (max m (linkl-max-let-depth (run-linkl r))))
                  saw-zero-pos-toplevel?)]
          [(s-exp)
           (define e
             (deshadow-linklet
              root-phase
              `(linklet ,(list* (if any-syntax-literals? '(.get-syntax-literal!) '())
                                (if any-transformer-registers? '(.set-transformer!) '())
                                ordered-importss)
                   ,(if export?
                        (hash-keys (hash-ref phase-defined-names root-phase '()))
                        '())
                 ,@body)))
           (s-exp->linklet module-name e)]))

      (values root-phase new-linkl)))

  (when (current-merged-output-file)
    (call-with-output-file*
     (current-merged-output-file)
     #:exists 'truncate
     (lambda (o)
       (define (path/submod->mpi path/submod)
         (define pos (hash-ref external-path-pos path/submod))
         (define path (if (pair? path/submod) (car path/submod) path/submod))
         (define mpi (list-ref all-mpis pos))
         (if (pair? path/submod)
             (module-path-index-join `(submod "." ,@(cdr path/submod)) mpi)
             mpi))
       (for ([root-phase (in-list (hash-keys body-linkl-ht))])
         (pretty-print root-phase o)
         (pretty-write (linklet->s-exp (hash-ref body-linkl-ht root-phase)) o))
       (pretty-print 'requires o)
       (pretty-print (for/hasheqv ([phase (in-list sorted-phases)])
                       (values (- phase)
                               (for/list ([path/submod+phase (in-list excluded-modules-to-require)]
                                          #:when (eqv? phase (cdr path/submod+phase)))
                                 (define path/submod (car path/submod+phase))
                                 (path/submod->mpi path/submod))))
                     o)
       (pretty-print 'phase-to-link-modules o)
       (pretty-print (for/hasheqv ([(root-phase import-keys) (in-hash phase-import-keys)])
                       (values root-phase
                               (for/list ([path/submod+phase (in-list import-keys)])
                                 (define path/submod (car path/submod+phase))
                                 (list (path/submod->mpi path/submod)
                                       (cdr path/submod+phase)))))
                     o))))

  (define metadata-ht
    (hasheq 'data data-linkl
            'decl decl-linkl
            'name name
            'min-phase min-phase
            'max-phase max-phase
            'portal-stxes portal-stxes
            'vm (case linkl-mode
                  [(linkl) #"racket"]
                  [(s-exp) #"linklet"]
                  [else (error "internal error: unrecognized linklet-representation mode")])))

  (define metadata-ht/stx
    (cond
      [serialized-stx
       (define stx-data-linklet (build-stx-data-linklet stx-vec serialized-stx))
       (define stx-linklet (build-stx-linklet stx-vec))
       (hash-set (hash-set metadata-ht 'stx-data stx-data-linklet)
                 'stx
                 stx-linklet)]
      [else
       ;; By not including a 'stx-data linklet, we get a default
       ;; linklet that supplies #f for any syntax-literal reference.
       metadata-ht]))

  ;; Merge metadat and phase-level-specific body linklets:
  (define bundle-ht
    (for/fold ([ht metadata-ht/stx]) ([(k v) (in-hash body-linkl-ht)])
      (hash-set ht k v)))

  (linkl-bundle bundle-ht))
