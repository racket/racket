#lang racket/base
(require ffi/unsafe
         (only-in '#%unsafe
                  unsafe-add-post-custodian-shutdown
                  unsafe-start-atomic
                  unsafe-end-atomic)
         "atomic.rkt")

(provide allocator deallocator retainer 
         (rename-out [deallocator releaser]))

(define allocated (make-late-weak-hasheq))

;; A `node` is used to implement a doubly linked list that
;; records the allocation order. That way, deallocators can
;; be called in reverse order when a non-main place exits.
(struct node (weak-val proc [next #:mutable] [prev #:mutable] rest)
  #:authentic)

;; A way to show all still-unfinalized values on exit:
#;
(plumber-add-flush! (current-plumber)
                    (lambda (v)
                      (for ([(k v) (in-hash allocated)])
                        (printf "~s\n" k))))

(define (deallocate v)
  ;; Called either as finalizer (in the finalizer thread) or
  ;; as a place is about to exit. Run in atomic mode to
  ;; avoid a race with a place exit.
  (unsafe-start-atomic)
  (let ([ds (hash-ref allocated v #f)])
    (when ds
      (hash-remove! allocated v)
      (let loop ([ds ds])
        (when ds
          (remove-node! ds)
          ((node-proc ds) v)
          (loop (node-rest ds))))))
  (unsafe-end-atomic))

(define (deallocate-one v expected-ds)
  ;; Called for a place exit.
  (let ([ds (hash-ref allocated v #f)])
    (cond
      [(eq? ds expected-ds)
       (define rest-ds (node-rest ds))
       (if rest-ds
           (hash-set! allocated v rest-ds)
           (hash-remove! allocated v))
       (remove-node! ds)
       ((node-proc ds) v)]
      [else
       ;; Not the expected node. Maybe an allocator
       ;; replaced existing allocations/retains.
       (remove-node! expected-ds)])))

(define (check-arity-includes-1 who proc [expected "(-> any/c any)"])
  (unless (procedure-arity-includes? proc 1)
    (raise-argument-error who expected proc)))

(define ((allocator d) proc)
  (check-arity-includes-1 'allocator d)
  (rename
   (let-values ([(_ allowed-kws) (procedure-keywords proc)])
     (define (register v)
       (when v
         (define ds (node (make-late-weak-box v) d #f #f #f))
         (add-node! ds)
         (hash-set! allocated v ds)
         (register-finalizer v deallocate))
       v)
     (cond
       [(null? allowed-kws)
        (lambda args
          (call-as-atomic
           (lambda ()
             (register (apply proc args)))))]
       [else
        (make-keyword-procedure
         (λ (kws kw-args . rest)
           (call-as-atomic
            (lambda ()
              (register (keyword-apply proc kws kw-args rest))))))]))
   proc))

(define ((deallocator [get-arg car]) proc)
  (check-arity-includes-1 'deallocator get-arg "(-> list/c any/c)")
  (rename
   (let-values ([(_ allowed-kws) (procedure-keywords proc)])
     (define (handle v)
       (let ([ds (hash-ref allocated v #f)])
         (when ds
           (remove-node! ds)
           (define rest-ds (node-rest ds))
           (if rest-ds
               (hash-set! allocated v rest-ds)
               (hash-remove! allocated v)))))
     (cond
       [(null? allowed-kws)
        (lambda args
          (call-as-atomic
           (lambda ()
             (begin0 (apply proc args)
                     (handle (get-arg args))))))]
       [else
        (make-keyword-procedure
         (λ (kws kw-args . rest)
           (call-as-atomic
            (lambda ()
              (begin0
                (keyword-apply proc kws kw-args rest)
                (handle (get-arg rest)))))))]))
   proc))

(define ((retainer d [get-arg car]) proc)
  (check-arity-includes-1 'retainer d)
  (check-arity-includes-1 'retainer get-arg "(-> list/c any/c)")
  (rename
   (let-values ([(_ allowed-kws) (procedure-keywords proc)])
     (define (handle v)
       (define next-ds (hash-ref allocated v #f))
       (define ds (node (make-late-weak-box v) d #f #f next-ds))
       (add-node! ds)
       (hash-set! allocated v ds)
       (unless next-ds
         (register-finalizer v deallocate)))
     (cond
       [(null? allowed-kws)
        (lambda args
          (call-as-atomic
           (lambda ()
             (begin0 (apply proc args)
                     (handle (get-arg args))))))]
       [else
        (make-keyword-procedure
         (λ (kws kw-args . rest)
           (call-as-atomic
            (lambda ()
              (begin0
                (keyword-apply proc kws kw-args rest)
                (handle (get-arg rest)))))))]))
   proc))


(define (rename new orig)
  (and orig
       (let-values ([(required-kws allowed-kws) (procedure-keywords orig)]
                    [(arity-mask) (procedure-arity-mask orig)]
                    [(name) (object-name orig)])
         (cond
           [(null? allowed-kws)
            (procedure-reduce-arity-mask
             new
             arity-mask
             name)]
           [else
            (procedure-reduce-keyword-arity-mask
             (if name (procedure-rename new name) new)
             arity-mask
             required-kws
             allowed-kws)]))))

;; ----------------------------------------

(define all-nodes #f)

(define (add-node! ds)
  (set-node-next! ds all-nodes)
  (when all-nodes
    (set-node-prev! all-nodes ds))
  (set! all-nodes ds))

(define (remove-node! ds)
  (define prev (node-prev ds))
  (define next (node-next ds))
  (if prev
      (set-node-next! prev next)
      (set! all-nodes next))
  (when next
    (set-node-prev! next prev)))

(define (release-all)
  (define ds all-nodes)
  (when ds
    (define v (weak-box-value (node-weak-val ds)))
    (cond
      [v (deallocate-one v ds)]
      [else
       (log-error "ffi/unsafe/alloc: internal error with a value deallocated by ~s" (node-proc ds))
       (remove-node! ds)])
    (release-all)))

;; This is a no-op in the main place:
(unsafe-add-post-custodian-shutdown release-all)
