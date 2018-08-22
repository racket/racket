#lang racket/base
(require (only-in '#%linklet primitive->compiled-position)
         racket/set
         compiler/zo-structs
         "run.rkt"
         "name.rkt")

(provide wrap-bundle)

(define (wrap-bundle body internals lifts excluded-module-mpis get-merge-info name)
  (define-values (runs
                  import-keys
                  ordered-importss
                  import-shapess
                  any-syntax-literals?
                  any-transformer-registers?
                  saw-zero-pos-toplevel?)
    (get-merge-info))

  (define module-name 'demodularized)
  (define (primitive v)
    (primval (or (primitive->compiled-position v)
                 (error "cannot find primitive" v))))
  
  (define new-linkl
    (linkl module-name
           (list* (if any-syntax-literals? '(.get-syntax-literal!) '())
                  (if any-transformer-registers? '(.set-transformer!) '())
                  ordered-importss)
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
           saw-zero-pos-toplevel?))

  (define data-linkl
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
                           ;; Construct two vectors: one for mpi construction, and
                           ;; another for selecting the slots that are externally referenced
                           ;; mpis (where the selection vector matches th `import-keys` order).
                           ;; If all import keys are primitive modules, then we just make
                           ;; a vector with those specs in order, but if there's a more
                           ;; complex mpi, then we have to insert extra slots in the first
                           ;; vector to hold intermediate mpi constructions.
                           ;; We could do better here by sharing common tails.
                           (let loop ([import-keys import-keys]
                                      [specs (list (box module-name))]
                                      [results (list 0)])
                             (cond
                               [(null? import-keys)
                                (list (list->vector (reverse specs))
                                      (list->vector (reverse results)))]
                               [else
                                (define path/submod+phase (car import-keys))
                                (define path (car path/submod+phase))
                                (cond
                                  [(symbol? path)
                                   (loop (cdr import-keys)
                                         (cons (vector `(quote ,path)) specs)
                                         (cons (length specs) results))]
                                  [(path? path)
                                   (define-values (i new-specs)
                                     (begin
                                     (let mpi-loop ([mpi (hash-ref excluded-module-mpis path)])
                                       (define-values (name base) (module-path-index-split mpi))
                                       (cond
                                         [(and (not name) (not base))
                                          (values 0 specs)]
                                         [(not base)
                                          (values (length specs) (cons (vector name) specs))]
                                         [else
                                          (define-values (next-i next-specs) (mpi-loop base))
                                          (values (length next-specs) (cons (vector name next-i) next-specs))]))))
                                   (loop (cdr import-keys)
                                         new-specs
                                         (cons i results))]
                                  [else
                                   (error 'wrap-bundle "unrecognized import path shape: ~s" path)])])))))
           16
           #f))

  (define decl-linkl
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
             '(self-mpi requires provides phase-to-link-modules)
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
                                (list->vector
                                 (let loop ([phases (sort (set->list
                                                           (for/set ([path/submod+phase (in-list import-keys)])
                                                             (cdr path/submod+phase)))
                                                          <)])
                                   (cond
                                     [(null? phases) (list '())]
                                     [else
                                      (define phase (car phases))
                                      (define n (for/sum ([path/submod+phase (in-list import-keys)])
                                                  (if (eqv? phase (cdr path/submod+phase)) 1 0)))
                                      (append `(#:cons #:list ,(add1 n) ,(- 0 phase))
                                              (apply
                                               append
                                               (for/list ([path/submod+phase (in-list import-keys)]
                                                          [i (in-naturals 1)]
                                                          #:when (eqv? phase (cdr path/submod+phase)))
                                                 `(#:mpi ,i)))
                                              (loop (cdr phases)))])))))))
              (def-values (list (toplevel 0 (+ exports-pos 2) #f #f)) ; provides
                (application (primitive hasheqv) null))
              (def-values (list (toplevel 0 (+ exports-pos 3) #f #f)) ; phase-to-link-modules
                (let ([depth 2])
                  (application (primitive hasheqv)
                               (list 0
                                     (let ([depth (+ depth (length import-keys))])
                                       (application (primitive list)
                                                    (for/list ([path/submod+phase (in-list import-keys)]
                                                               [i (in-naturals 1)])
                                                      (let ([depth (+ depth 2)])
                                                        (application (toplevel depth module-use-pos #f #f)
                                                                     (list
                                                                      (let ([depth (+ depth 2)])
                                                                        (application (primitive vector-ref)
                                                                                     (list
                                                                                      (toplevel depth mpi-vector-pos #f #f)
                                                                                      i)))
                                                                      (cdr path/submod+phase))))))))))))
             (+ 32 (length import-keys))
             #f)))

  ;; By not including a 'stx-data linklet, we get a default
  ;; linklet that supplies #f for any syntax-literal reference.

  (linkl-bundle (hasheq 0 new-linkl
                        'data data-linkl
                        'decl decl-linkl
                        'name name)))
