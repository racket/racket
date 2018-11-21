#lang racket/base
(require "../host/linklet.rkt"
         "../namespace/namespace.rkt"
         "../compile/compiled-in-memory.rkt"
         "../compile/serialize.rkt"
         "../compile/eager-instance.rkt"
         "../compile/reserved-symbol.rkt"
         "../compile/namespace-scope.rkt"
         "../compile/multi-top.rkt"
         "../compile/linklet.rkt")

(provide create-compiled-in-memorys-using-shared-data)

(define (create-compiled-in-memorys-using-shared-data tops data-linklet ns)
  (define data-instance
    (instantiate-linklet data-linklet
                         (list deserialize-instance
                               (make-eager-instance-instance
                                #:namespace ns
                                #:dest-phase (namespace-phase ns)
                                #:self (namespace-mpi ns)
                                #:bulk-binding-registry (namespace-bulk-binding-registry ns)
                                #:inspector (current-code-inspector)))))
  
  (define (data key) (instance-variable-value data-instance key))
  
  (define mpi-vector (data mpi-vector-id))
  (define mpi-vector-trees (data 'mpi-vector-trees))
  (define phase-to-link-modules-vector (data 'phase-to-link-modules-vector))
  (define phase-to-link-modules-trees (data 'phase-to-link-modules-trees))
  (define syntax-literals (data 'syntax-literals))
  (define syntax-literals-trees (data 'syntax-literals-trees))
  
  (define namespace-scopes (extract-namespace-scopes ns))
  
  (define (construct-compiled-in-memory ld
                                        mpi-vector-tree
                                        phase-to-link-modules-tree
                                        syntax-literals-tree)
    (define is-module? (or (linklet-bundle? ld)
                           (let ([b (hash-ref (linklet-directory->hash ld) #f #f)])
                             (and b (hash-ref (linklet-bundle->hash b) 'decl #f)))))
    (define mpi-pos-vec (vector-ref mpi-vector-tree 0))
    (define syntax-literals-spec (vector-ref syntax-literals-tree 0))
    (define pres (if is-module? 
                     (extract-submodules ld 'pre)
                     (compiled-top->compiled-tops ld)))
    (define posts (if is-module?
                      (extract-submodules ld 'post)
                      null))
    (define (map-construct-compiled-in-memory l vec-pos)
      (for/list ([sub-ld (in-list l)]
                 [mpi-vector-tree (in-list (vector-ref mpi-vector-tree vec-pos))]
                 [phase-to-link-modules-tree (in-list (vector-ref phase-to-link-modules-tree vec-pos))]
                 [syntax-literals-tree (in-list (vector-ref syntax-literals-tree vec-pos))])
        (construct-compiled-in-memory sub-ld
                                      mpi-vector-tree
                                      phase-to-link-modules-tree
                                      syntax-literals-tree)))
    (compiled-in-memory ld
                        #f ; self
                        #f ; requires
                        #f ; provides
                        (vector-ref phase-to-link-modules-vector (vector-ref phase-to-link-modules-tree 0))
                        #f ; compile-time-inspector
                        #hasheqv() ; phase-to-link-extra-inspectorsss
                        (for/vector #:length (vector-length mpi-pos-vec) ([pos (in-vector mpi-pos-vec)])
                                    (vector-ref mpi-vector pos))
                        (for/vector #:length (cdr syntax-literals-spec) ([i (in-range (cdr syntax-literals-spec))])
                                    (and syntax-literals
                                         (vector-ref syntax-literals (+ (car syntax-literals-spec) i))))
                        (map-construct-compiled-in-memory pres 1)
                        (map-construct-compiled-in-memory posts 2)
                        namespace-scopes
                        #f))
  
  (map construct-compiled-in-memory
       tops
       mpi-vector-trees
       phase-to-link-modules-trees
       syntax-literals-trees))

;; ----------------------------------------
 
(define (extract-submodules ld names-key)
  (cond
   [(linklet-bundle? ld)
    ;; no submodules
    null]
   [else
    (define h (linklet-directory->hash ld))
    (define mod (hash-ref h #f #f))
    (unless mod (error "missing main module"))
    (define mh (linklet-bundle->hash mod))
    (define names (hash-ref mh names-key null))
    (for/list ([name (in-list names)])
      (hash-ref h name (lambda () (error "missing submodule declaration:" name))))]))
