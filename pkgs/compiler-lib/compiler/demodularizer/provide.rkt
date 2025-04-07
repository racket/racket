#lang racket/base
(require racket/phase+space
         "binding.rkt"
         "binding-lookup.rkt"
         "name.rkt"
         "module-path.rkt")

(provide provides-to-names
         gather-provides
         extend-provides)

(define (provides-to-names provides
                           names transformer-names
                           one-mods
                           excluded-module-mpis included-module-phases
                           #:keep-syntax? keep-syntax?)
  (for/fold ([ht #hasheqv()]) ([(phase+space binds) (in-hash provides)])
    (define root-phase (phase+space-phase phase+space))
    (hash-set ht
              root-phase
              (append
               (for/list ([bind (in-hash-values binds)]
                          #:unless (and (not keep-syntax?) (binding-syntax? bind))
                          #:do [(define-values (sym path/submod phase) (binding-sym-path/submod-phase bind))
                                (define-values (name at-phase)
                                  (binding-lookup path/submod phase sym
                                                  names transformer-names
                                                  one-mods
                                                  excluded-module-mpis included-module-phases))])
                 name)
               (hash-ref ht root-phase null)))))

;; provides gathered for use with `extend-provides`
(define (gather-provides all-provides
                         top-path/submod
                         provides
                         excluded-module-mpis
                         #:keep-syntax? keep-syntax?)
  (for/fold ([all-provides all-provides]) ([(phase+space binds) (in-hash provides)])
    (define root-phase (phase+space-phase phase+space))
    (for/fold ([all-provides all-provides]) ([bind (in-hash-values binds)]
                                             #:unless (and (not keep-syntax?) (binding-syntax? bind)))
      (define-values (sym path/submod phase) (binding-sym-path/submod-phase bind))
      (define mpi+phase (hash-ref excluded-module-mpis path/submod #f))
      (cond
        [mpi+phase
         (define new-path/submod (module-path-index->path/submod (car mpi+phase) top-path/submod))
         (hash-set all-provides new-path/submod (cons bind (hash-ref all-provides new-path/submod null)))]
        [else
         all-provides]))))

;; for a submodule synthesized to implement a pane, export anything that is ultimately
;; exported from the demodularized modules; otherwise, the bindings count as non-exported bindings
;; that are implicitly protected, and so use can trigger a sandbox error
(define (extend-provides provides all-provides top-path/submod included-module-phases)
  (unless (= 0 (hash-count provides))
    (error "expected no provides for a synthesized pane"))
  (for/fold ([provides provides]) ([bind (in-list (hash-ref all-provides top-path/submod null))])
    (define-values (sym path/submod phase) (binding-sym-path/submod-phase bind))
    (define new-phase (+ (hash-ref included-module-phases path/submod #f)
                         phase))
    (cond
      [new-phase
       (define ht (hash-ref provides new-phase #hash()))
       (if (hash-ref ht new-phase #f)
           provides
           (hash-set provides new-phase
                     (hash-set ht sym bind)))]
      [else
       (error "phase lookup failed for synthesized pane provide")])))
