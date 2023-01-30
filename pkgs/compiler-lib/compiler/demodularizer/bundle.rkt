#lang racket/base
(require (only-in '#%linklet primitive->compiled-position)
         racket/set
         compiler/zo-structs
         "run.rkt"
         "name.rkt"
         "linklet.rkt")

(provide wrap-bundle)

(define (wrap-bundle linkl-mode body internals lifts excluded-module-mpis get-merge-info name)
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
         `(linklet ,(list* (if any-syntax-literals? '(.get-syntax-literal!) '())
                           (if any-transformer-registers? '(.set-transformer!) '())
                           ordered-importss)
              () ; exports
            ,@body))
       (s-exp->linklet module-name e)]))

  (define serialized-mpis
    ;; Construct two vectors: one for mpi construction, and
    ;; another for selecting the slots that are externally referenced
    ;; mpis (where the selection vector matches the `import-keys` order).
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
           (for/set ([path/submod+phase (in-list import-keys)])
             (cdr path/submod+phase)))
          <))

  (define serialized-requires
    (list->vector
     (let loop ([phases sorted-phases])
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
                  (loop (cdr phases)))]))))

  (define recur-requires
    (for/list ([phase (in-list sorted-phases)])
      (for/list ([path/submod+phase (in-list import-keys)]
                 #:when (eqv? phase (cdr path/submod+phase)))
        #t)))

  (define (make-phase-to-link-modules make-apply
                                      get-prim
                                      get-module-use
                                      get-mpi-vector)
    (let ([depth 2])
      (make-apply (get-prim 'hasheqv hasheqv)
                  (list 0
                        (let ([depth (+ depth (length import-keys))])
                          (make-apply (get-prim 'list list)
                                      (for/list ([path/submod+phase (in-list import-keys)]
                                                 [i (in-naturals 1)])
                                        (let ([depth (+ depth 2)])
                                          (make-apply (get-module-use depth)
                                                       (list
                                                        (let ([depth (+ depth 2)])
                                                          (make-apply (get-prim 'vector-ref vector-ref)
                                                                      (list
                                                                       (get-mpi-vector depth)
                                                                       i)))
                                                        (cdr path/submod+phase)))))))))))
    
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
                '(self-mpi requires recur-requires provides phase-to-link-modules portal-stxes)
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
                 (def-values (list (toplevel 0 (+ exports-pos 3) #f #f)) ; provides
                   (application (primitive hasheqv) null))
                 (def-values (list (toplevel 0 (+ exports-pos 4) #f #f)) ; phase-to-link-modules
                   (make-phase-to-link-modules application
                                               (lambda (name prim) (primitive prim))
                                               (lambda (depth) (toplevel depth module-use-pos #f #f))
                                               (lambda (depth) (toplevel depth mpi-vector-pos #f #f))))
                 (def-values (list (toplevel 0 (+ exports-pos 5) #f #f)) ; portal-stxes
                   (application (primitive hasheqv) null)))
                (+ 32 (length import-keys))
                #f))]
      [(s-exp)
       (s-exp->linklet
        'decl
        `(linklet ((deserialize
                    module-use)
                   (.mpi-vector))
             (self-mpi requires recur-requires provides phase-to-link-modules portal-stxes)
           (define-values (self-mpi) (vector-ref .mpi-vector 0))
           (define-values (requires) (deserialize .mpi-vector #f #f 0 '#() 0 '#() '#()
                                                  (quote ,serialized-requires)))
           (define-values (recur-requires) (quote ,recur-requires))
           (define-values (provides) '#hasheqv())
           (define-values (phase-to-link-modules)
             ,(make-phase-to-link-modules cons
                                          (lambda (name prim) name)
                                          (lambda (depth) 'module-use)
                                          (lambda (depth) '.mpi-vector)))
           (define-values (portal-stxes) '#hasheqv())))]))

  ;; By not including a 'stx-data linklet, we get a default
  ;; linklet that supplies #f for any syntax-literal reference.

  (linkl-bundle (hasheq 0 new-linkl
                        'data data-linkl
                        'decl decl-linkl
                        'name name
                        'vm (case linkl-mode
                              [(linkl) #"racket"]
                              [(s-exp) #"linklet"]
                              [else (error "internal error: unrecognized linklet-representation mode")]))))

