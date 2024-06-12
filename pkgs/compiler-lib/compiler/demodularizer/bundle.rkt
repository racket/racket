#lang racket/base
(require racket/set
         racket/list
         compiler/zo-structs
         racket/pretty
         syntax/modcollapse
         racket/phase+space
         "run.rkt"
         "name.rkt"
         "linklet.rkt"
         "syntax.rkt"
         "import.rkt"
         "binding.rkt"
         "deshadow.rkt"
         "merged.rkt"
         "at-phase-level.rkt")

(provide wrap-bundle)

(define (wrap-bundle module-name phase-merged name-imports
                     stx-vec portal-stxes
                     excluded-modules-to-require excluded-module-mpis included-module-phases
                     provides
                     names transformer-names one-mods
                     #:import/export-only import/export-only
                     #:pre-submodules pre-submodules
                     #:post-submodules post-submodules
                     #:dump-output-file dump-output-file)

  (define-values (min-phase max-phase)
    (for/fold ([min-phase 0] [max-phase 0]) ([phase (in-hash-keys phase-merged)])
      (values (min phase min-phase) (max phase max-phase))))

  (define self-mpi (module-path-index-join #f #f))

  ;; Gather all paths that are either leftover imports to a linklet,
  ;; required overall as excluded modules, or mentioned in a provided
  ;; binding. Leftover imports and provides should be transitively
  ;; required by the excluded modules, but we don't try to check that.
  (define-values (external-path-pos external-mpis)
    (let ()
      (define (add-path step path/submod+phase ht simple-ht rev-paths)
        (cond
          [(hash-ref ht path/submod+phase #f)
           (values ht simple-ht rev-paths)]
          [else
           (define path/submod (car path/submod+phase))
           (define phase (cdr path/submod+phase))
           (define mpi+phase (or (hash-ref excluded-module-mpis path/submod #f)
                                 (hash-ref excluded-module-mpis (at-phase-level path/submod phase) #f)
                                 (and (symbol? path/submod)
                                      (cons (module-path-index-join `(quote ,path/submod) #f) 0))
                                 (error 'import-mpis "cannot find module: ~s" path/submod)))
           (define mpi (car mpi+phase))
           ;; collapse to a simplified MPI
           (define simple-path (collapse-module-path-index mpi))
           (define new-mpi (module-path-index-join simple-path self-mpi))
           (cond
             [(hash-ref simple-ht simple-path #f)
              => (lambda (pos)
                   (values (hash-set ht path/submod+phase pos)
                           simple-ht
                           rev-paths))]
             [else
              (define pos (add1 (hash-count simple-ht)))
              (values (hash-set ht path/submod+phase pos)
                      (hash-set simple-ht simple-path pos)
                      (cons new-mpi rev-paths))])]))
      (define-values (import-ht import-simple-ht import-rev-paths)
        (for*/fold ([ht #hash()] [simple-ht #hash()] [rev-paths '()])
                   ([mgd (in-hash-values phase-merged)]
                    [new-name (in-hash-keys (merged-used-import-names mgd))])
          (define i (hash-ref name-imports new-name))
          (add-path 'import (import-path/submod+phase i) ht simple-ht rev-paths)))
      (define-values (require-ht require-simple-ht require-rev-paths)
        (for*/fold ([ht import-ht] [simple-ht import-simple-ht] [rev-paths import-rev-paths])
                   ([path/submod+phase-shift (in-hash-keys excluded-modules-to-require)])
          (define path/submod (car path/submod+phase-shift))
          (define phase-shift (cdr path/submod+phase-shift))
          (add-path 'require (cons path/submod (- phase-shift)) ht simple-ht rev-paths)))
      (define-values (provide-ht provide-simple-ht provide-rev-paths)
        (for*/fold ([ht require-ht] [simple-ht require-simple-ht] [rev-paths require-rev-paths])
                   ([binds (in-hash-values provides)]
                    [bind (in-hash-values binds)]
                    [mpi+phase (in-list (binding-mpi+phases bind))]
                    #:do [(define mpi (car mpi+phase))
                          (define phase (cdr mpi+phase))
                          (define r (module-path-index-resolve mpi))
                          (define path/submod (resolved-module-path-name r))]
                    #:when (or (symbol? path/submod)
                               (hash-ref excluded-module-mpis path/submod #f)
                               (hash-ref excluded-module-mpis (at-phase-level path/submod phase) #f)))
          (add-path 'provide (cons path/submod phase) ht simple-ht rev-paths)))
      (values provide-ht
              (reverse provide-rev-paths))))

  (define-values (all-mpis serialized-stx)
    (serialize-syntax stx-vec self-mpi
                      external-mpis excluded-module-mpis included-module-phases
                      names transformer-names one-mods))

  (define serialized-mpis
    ;; Construct two vectors: one for mpi construction, and
    ;; another for selecting the slots that are externally referenced
    ;; mpis (where the selection vector matches the `external-mpis` order
    ;; followed by `stx-mpis` in order).
    ;; If all module paths refer to symbol-named primitive modules, then 
    ;; we just make a vector with those specs in order, but if there's a
    ;; more complex mpi, then we have to insert extra slots in the first
    ;; vector to hold intermediate mpi constructions.
    ;; We could do better here by sharing common tails.
    (let loop ([external-mpis external-mpis]
               [all-mpis (cdr all-mpis)] ; cdr skips self mpi
               [specs (list (box module-name))] ; initial spec = self mpi
               [results (list 0)])              ; initial 0 = self mpi
      (define (mpi-loop mpi specs)
        (define-values (name base) (module-path-index-split mpi))
        (cond
          [(and (not name) (not base))
           (values 0 specs)]
          [(not base)
           (values (length specs) (cons (if (symbol? name)
                                            (vector `(quote ,name))
                                            (vector name))
                                        specs))]
          [else
           (define-values (next-i next-specs) (mpi-loop base specs))
           (values (length next-specs) (cons (vector name next-i) next-specs))]))
      (cond
        [(null? external-mpis)
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
         (define-values (i new-specs) (mpi-loop (car all-mpis) specs))
         (loop (cdr external-mpis)
               (cdr all-mpis)
               new-specs
               (cons i results))])))

  (define data-linkl
    (s-exp->linklet
     'data
     `(linklet ((deserialize-module-path-indexes))
          (.mpi-vector)
        (define-values (.mpi-vector)
          (deserialize-module-path-indexes (quote ,(car serialized-mpis))
                                           (quote ,(cadr serialized-mpis)))))))

  ;; When a require of X turns into a require of pane Y with a phase shift,
  ;; then we need to both change X to Y and move the require to the right phase.
  ;; Also, we want to avoid duplicate requires of the same Y from different Xs.
  (define phase->require-poss ; phase -> (hash pos ...)
    (for/fold ([phase->require-poss #hasheqv()])
              ([path/submod+phase-shift (in-hash-keys excluded-modules-to-require)])
      (define path/submod (car path/submod+phase-shift))
      (define phase-shift (cdr path/submod+phase-shift))
      (define maybe-mpi+phase (or (hash-ref excluded-module-mpis path/submod #f)
                                  (hash-ref excluded-module-mpis (at-phase-level path/submod (- phase-shift)) #f)))
      (define new-phase-shift (if maybe-mpi+phase
                                  (- phase-shift (cdr maybe-mpi+phase))
                                  phase-shift))
      (define pos (or (hash-ref external-path-pos (cons path/submod (- phase-shift)) #f)
                      (raise-arguments-error 'bundle "cannot find position for require"
                                             "in" module-name
                                             "require" (cons path/submod (- phase-shift))
                                             "new phase shift" new-phase-shift)))
      (hash-update phase->require-poss new-phase-shift
                   (lambda (poss) (hash-set poss pos #t))
                   #hasheqv())))

  (define sorted-phases
    (sort (hash-keys phase->require-poss) <))

  (define serialized-requires
    (list->vector
     (let loop ([phases sorted-phases])
       (cond
         [(null? phases) (list '())]
         [else
          (define phase (car phases))
          (define poss (hash-keys (hash-ref phase->require-poss phase) #t))
          (define n (length poss))
          (append `(#:cons #:list ,(add1 n) ,phase)
                  (apply
                   append
                   (for/list ([pos (in-list poss)])
                     `(#:mpi ,pos)))
                  (loop (cdr phases)))]))))

  (define recur-requires
    (for/list ([phase (in-list sorted-phases)])
      (for/list ([i (in-range (hash-count (hash-ref phase->require-poss phase)))])
        #t)))

  (define serialized-provides
    (let ([phase+spaces (hash-keys provides)]) ; deterministic output would need sorting here
      (list->vector
       `(#:hasheqv ,(hash-count provides)
         ,@(apply
            append
            (for/list ([phase+space (in-list phase+spaces)])
              (define phase (phase+space-phase phase+space))
              (define ht (hash-ref provides phase+space))
              `(,@(if (pair? phase+space)
                      `(#:cons ,phase ,(phase+space-space phase+space))
                      (list phase+space))
                #:hasheq
                ,(hash-count ht)
                ,@(apply
                   append
                   (for/list ([(name bind) (in-hash ht)])
                     `(,name ,@(serialize-binding bind phase
                                                  external-path-pos excluded-module-mpis included-module-phases
                                                  names transformer-names one-mods
                                                  (length all-mpis))))))))))))

  (define (path/submod+phase->mpi-pos+phase path/submod+phase)
    (define path/submod (car path/submod+phase))
    (define phase (cdr path/submod+phase))
    (define maybe-mpi+phase (or (hash-ref excluded-module-mpis path/submod #f)
                                (hash-ref excluded-module-mpis (at-phase-level path/submod phase) #f)))
    (define phase-shift (if maybe-mpi+phase (cdr maybe-mpi+phase) 0))
    (cons (hash-ref external-path-pos path/submod+phase) (+ phase phase-shift)))

  (define phase-import-keys
    (for/hasheqv ([(root-phase mgd) (in-hash phase-merged)])
      (define used-import-names (merged-used-import-names mgd))
      (define import-keys ; (list (cons path/submod phase) ...)
        (hash-keys
         (for/hash ([name (in-hash-keys used-import-names)]
                    #:do [(define i (hash-ref name-imports name))]
                    #:when (or (not import/export-only)
                               (hash-ref import/export-only (import-name i) #f)))
           (values (path/submod+phase->mpi-pos+phase (import-path/submod+phase i))
                   #t))))
      (values root-phase import-keys)))

  (define phase-importss
    (for/hasheqv ([(root-phase mgd) (in-hash phase-merged)])
      (define used-import-names (merged-used-import-names mgd))
      (define key-imports
        (for/fold ([ht #hash()]) ([name (in-hash-keys used-import-names)])
          (define i (hash-ref name-imports name))
          (cond
            [(or (not import/export-only)
                 (hash-ref import/export-only (import-name i) #f))
             (hash-update ht
                          (path/submod+phase->mpi-pos+phase (import-path/submod+phase i))
                          (lambda (imports)
                            (cons
                             (if (eq? (import-name i) (import-src-ext-name i))
                                 (import-name i)
                                 (list (import-src-ext-name i) (import-name i)))
                             imports))
                          null)]
            [else ht])))
      (values root-phase
              (for/list ([import-key (in-list (hash-ref phase-import-keys root-phase))])
                (hash-ref key-imports import-key null)))))

  (define phase-to-link-modules
    `(hasheqv ,@(apply
                 append
                 (for/list ([(root-phase import-keys) (in-hash phase-import-keys)])
                   (list root-phase
                         `(list ,@(for/list ([mpi-pos+phase (in-list import-keys)])
                                    (define pos (car mpi-pos+phase))
                                    `(module-use (vector-ref .mpi-vector ,pos)
                                                 ,(cdr mpi-pos+phase)))))))))

  (define decl-linkl
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
          ,phase-to-link-modules)
        (define-values (portal-stxes) ',portal-stxes))))

  (define body-linkl-ht
    (for/hasheqv ([(root-phase mgd) (in-hash phase-merged)])
      (define import-keys (hash-ref phase-import-keys root-phase))
      (define ordered-importss (hash-ref phase-importss root-phase))

      (define body (merged-body mgd))
      (define any-syntax-literals? (merged-any-syntax-literals? mgd))
      (define any-transformer-registers? (merged-any-transformer-registers? mgd))
      (define defined-names (merged-defined-names mgd))

      (define new-linkl
        (s-exp->linklet
         'module
         (deshadow-linklet
          root-phase
          `(linklet ,(list* (if any-syntax-literals? '(.get-syntax-literal!) '())
                            (if any-transformer-registers? '(.set-transformer!) '())
                            ordered-importss)
               ,(cond
                  [(not import/export-only)
                   (hash-keys defined-names)]
                  [else
                   (for/list ([k (in-hash-keys defined-names)]
                              #:when (hash-ref import/export-only k #f))
                     k)])
             ,@body))))

      (values root-phase new-linkl)))

  (when dump-output-file
    (call-with-output-file*
     dump-output-file
     #:exists 'append
     (lambda (o)
       (display "-------------------\n" o)
       (pretty-write module-name o)
       (for ([root-phase (in-list (hash-keys body-linkl-ht))])
         (pretty-print root-phase o)
         (pretty-write (linklet->s-exp (hash-ref body-linkl-ht root-phase)) o))
       (pretty-write 'requires o)
       (pretty-write (for/hasheqv ([phase (in-list sorted-phases)])
                       (values phase
                               (for/list ([pos (in-list (hash-keys (hash-ref phase->require-poss phase)
                                                                   #t))])
                                 (list-ref all-mpis pos))))
                     o)
       (pretty-write 'phase-to-link-modules o)
       (pretty-write (for/hasheqv ([(root-phase import-keys) (in-hash phase-import-keys)])
                       (values root-phase
                               (for/list ([mpi-pos+phase (in-list import-keys)])
                                 (define mpi-pos (car mpi-pos+phase))
                                 (list (list-ref all-mpis mpi-pos)
                                       (cdr mpi-pos+phase)))))
                     o))))

  (define metadata-ht
    (let* ([metadata-ht
            (hasheq 'data data-linkl
                    'decl decl-linkl
                    'name module-name
                    'min-phase min-phase
                    'max-phase max-phase
                    'vm  #"linklet")]
           [metadata-ht (if (null? pre-submodules)
                            metadata-ht
                            (hash-set metadata-ht 'pre pre-submodules))]
           [metadata-ht (if (null? post-submodules)
                            metadata-ht
                            (hash-set metadata-ht 'post post-submodules))])
      metadata-ht))
      
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
