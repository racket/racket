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

(define ((allocator d) proc)
  (rename
    (lambda args
      (call-as-atomic
        (lambda ()
          (let ([v (apply proc args)])
            (when v
              (define ds (node (make-late-weak-box v) d #f #f #f))
              (add-node! ds)
              (hash-set! allocated v ds)
              (register-finalizer v deallocate))
            v))))
    proc))

(define ((deallocator [get-arg car]) proc)
  (rename
    (lambda args
      (call-as-atomic
        (lambda ()
         (apply proc args)
         (let ([v (get-arg args)])
           (let ([ds (hash-ref allocated v #f)])
             (when ds
               (remove-node! ds)
               (define rest-ds (node-rest ds))
               (if rest-ds
                   (hash-set! allocated v rest-ds)
                   (hash-remove! allocated v))))))))
    proc))

(define ((retainer d [get-arg car]) proc)
  (rename
   (lambda args
     (call-as-atomic
       (lambda ()
         (begin0
           (apply proc args)
           (let ([v (get-arg args)])
             (define next-ds (hash-ref allocated v #f))
             (define ds (node (make-late-weak-box v) d #f #f next-ds))
             (add-node! ds)
             (hash-set! allocated v ds)
             (unless next-ds
               (register-finalizer v deallocate)))))))
   proc))

(define (rename new orig)
  (and orig
       (let ([n (object-name orig)]
             [new (procedure-reduce-arity
                   new
                   (procedure-arity orig))])
         (if n
             (procedure-rename new n)
             new))))

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
