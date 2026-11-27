#lang racket/base
(require "../common/module-path.rkt"
         "../namespace/module.rkt"
         "../namespace/namespace.rkt")

(provide make-module-path-index-rewriter)

;; A compiled module can refer to a module through a module path index
;; that isn't relative to the module's own "self" and that is rooted
;; at a filesystem path. That happens when the reference comes from a
;; shared (i.e., cross-phase persistent) instance whose module path
;; index was created for some other requester. Serializing such a
;; filesystem path would make the compiled module depend on where it
;; was compiled, so this rewriter replaces the filesystem-rooted part
;; with a chain through the module's own requires, if one leads to the
;; same module. Besides requires, the traversal follows modules that a
;; cross-phase persistent instance loads on demand (as registered via
;; `variable-reference-add-attach-dependency!`, such as by `lazy-require`),
;; since a macro from such a module can contribute references. The
;; chain is found by a breadth-first traversal of module declarations,
;; so it's a shortest one, and the traversal happens only if some
;; reference needs it.
(define (make-module-path-index-rewriter self requires ns)
  (define found (make-hasheq)) ; resolved-module-path -> mpi relative to `self`
  (define queue null) ; reversed list of mpi+name pairs to explore
  (define pending null) ; list of mpi+name pairs to explore, in order
  (define (add! mpi)
    (define name (with-handlers ([exn:fail? (lambda (exn) #f)])
                   (module-path-index-resolve mpi)))
    (when (and name (not (hash-ref found name #f)))
      (hash-set! found name mpi)
      (set! queue (cons (cons mpi name) queue))))
  (for* ([phase+mpis (in-list requires)]
         [mpi (in-list (cdr phase+mpis))])
    (add! mpi))
  (define (explore-next!)
    (when (null? pending)
      (set! pending (reverse queue))
      (set! queue null))
    (cond
      [(null? pending) #f]
      [else
       (define mpi+name (car pending))
       (set! pending (cdr pending))
       (define mpi (car mpi+name))
       (define m (namespace->module ns (cdr mpi+name)))
       (when m
         (define (add-required! req-mpi)
           (add! (module-path-index-shift req-mpi (module-self m) mpi)))
         (cond
           [(module-flattened-requires m)
            => (lambda (mpi+phasess)
                 (for ([mpi+phases (in-list mpi+phasess)])
                   (add-required! (vector-ref mpi+phases 0))))]
           [else
            (for* ([phase+mpis (in-list (module-requires m))]
                   [req-mpi (in-list (cdr phase+mpis))])
              (add-required! req-mpi))])
         ;; Modules loaded on demand by a shared instance; each is
         ;; normally relative to the instance's module path index, so
         ;; shift it to the chain that reaches the instance
         (when (module-cross-phase-persistent? m)
           (define mi (namespace->module-instance ns (cdr mpi+name) 0))
           (when mi
             (define inst-mpi (namespace-mpi (module-instance-namespace mi)))
             (for ([dep (in-list (module-instance-attach-dependencies mi))])
               (define shifted (module-path-index-shift dep inst-mpi mpi))
               (unless (eq? shifted dep)
                 (add! shifted))))))
       #t]))
  (define (lookup name)
    (let loop ()
      (or (hash-ref found name #f)
          (and (explore-next!)
               (loop)))))
  (define rewritten (make-hasheq))
  (lambda (mpi)
    (let rewrite ([mpi mpi])
      (or (hash-ref rewritten mpi #f)
          (let ([new-mpi
                 (let-values ([(path base) (module-path-index-split mpi)])
                   (cond
                     [(not path) mpi]
                     [(filesystem-module-path? path)
                      (define name (with-handlers ([exn:fail? (lambda (exn) #f)])
                                     (module-path-index-resolve mpi)))
                      (or (and name (lookup name))
                          mpi)]
                     [(module-path-index? base)
                      (define new-base (rewrite base))
                      (if (eq? new-base base)
                          mpi
                          (module-path-index-join* path new-base))]
                     [else mpi]))])
            (hash-set! rewritten mpi new-mpi)
            new-mpi)))))

(define (filesystem-module-path? path)
  (cond
    [(path? path) (complete-path? path)]
    [(and (pair? path) (eq? 'file (car path))) (complete-path? (cadr path))]
    [(and (pair? path) (eq? 'submod (car path))) (filesystem-module-path? (cadr path))]
    [else #f]))
