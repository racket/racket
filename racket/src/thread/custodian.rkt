#lang racket/base
(require "custodian-object.rkt"
         "place-object.rkt"
         "check.rkt"
         "atomic.rkt"
         "host.rkt"
         "evt.rkt"
         "semaphore.rkt"
         "parameter.rkt")

(provide current-custodian
         make-custodian
         custodian?
         custodian-shutdown-all
         custodian-managed-list
         make-custodian-box
         custodian-box?
         custodian-box-value
         custodian-memory-accounting-available?
         custodian-require-memory
         custodian-limit-memory
         custodian-shut-down?

         custodian-subordinate?
         custodian-manages-reference?
         custodian-reference->custodian
         unsafe-make-custodian-at-root
         unsafe-custodian-register
         unsafe-custodian-unregister
         custodian-register-thread
         custodian-register-place
         raise-custodian-is-shut-down
         set-post-shutdown-action!
         check-queued-custodian-shutdown
         set-place-custodian-procs!
         custodian-check-immediate-limit)

(module+ scheduling
  (provide do-custodian-shutdown-all
           set-root-custodian!
           create-custodian))

;; For `(struct custodian ...)`, see "custodian-object.rkt"

(struct custodian-box ([v #:mutable] sema)
  #:authentic
  #:property prop:evt (lambda (cb)
                        (wrap-evt (custodian-box-sema cb) (lambda (v) cb))))

(struct willed-callback (proc will)
  #:property prop:procedure (struct-field-index proc)
  #:authentic)

(struct at-exit-callback willed-callback ()
  #:authentic)

;; Reporting registration in a custodian through this indirection
;; enables GCing custodians that aren't directly referenced, merging
;; the managed objects into the parent, although that posisbility is
;; not currently implemented
(struct custodian-reference (c)
  #:authentic)

(define/who current-custodian
  (make-parameter root-custodian
                  (lambda (v)
                    (check who custodian? v)
                    v)))

;; To initialize a new place:
(define (set-root-custodian! c)
  (set! root-custodian c)
  (current-custodian c))

(define/who (make-custodian [parent (current-custodian)])
  (check who custodian? parent)
  (define c (create-custodian))
  (set-custodian-place! c (custodian-place parent))
  (define cref (do-custodian-register parent c do-custodian-shutdown-all #f #t #t))
  (set-custodian-parent-reference! c cref)
  (unless cref (raise-custodian-is-shut-down who parent))
  c)

(define (unsafe-make-custodian-at-root)
  (make-custodian root-custodian))

;; The given `callback` will be run in atomic mode.
;; Unless `weak?` is true, the given `obj` is registered with an ordered
;; finalizer, so don't supply an `obj` that is exposed to safe code
;; that might see `obj` after finalization through a weak reference
;; (and detect that `obj` is thereafter retained strongly).
(define (do-custodian-register cust obj callback at-exit? weak? gc-root?)
  (atomically
   (cond
     [(custodian-shut-down? cust) #f]
     [else
      (define we (and (not weak?)
                      (host:make-stubborn-will-executor void)))
      (hash-set! (custodian-children cust)
                 obj
                 (cond
                   [weak? callback]
                   [at-exit? (at-exit-callback callback we)]
                   [else (willed-callback callback we)]))
      (when we
        ;; Registering with a will executor that we never poll has the
        ;; effect of turning a weak reference into a strong one when
        ;; there are no other references:
        (host:will-register we obj void))
      (when gc-root?
        (host:disable-interrupts)
        (unless (custodian-gc-roots cust)
          (set-custodian-gc-roots! cust (make-weak-hasheq)))
        (hash-set! (custodian-gc-roots cust) obj #t)
        (host:enable-interrupts))
      (custodian-reference cust)])))

(define (unsafe-custodian-register cust obj callback at-exit? weak?)
  (do-custodian-register cust obj callback at-exit? weak? #f))

(define (custodian-register-thread cust obj callback)
  (do-custodian-register cust obj callback #f #t #t))

(define (custodian-register-place cust obj callback)
  (do-custodian-register cust obj callback #f #t #t))

(define (unsafe-custodian-unregister obj cref)
  (when cref
    (atomically
     (define c (custodian-reference-c cref))
     (unless (custodian-shut-down? c)
       (hash-remove! (custodian-children c) obj))
     (host:disable-interrupts)
     (define gc-roots (custodian-gc-roots c))
     (when gc-roots
       (hash-remove! gc-roots obj))
     (host:enable-interrupts))))

;; Hook for thread scheduling:
(define post-shutdown-action void)
(define (set-post-shutdown-action! proc)
  (set! post-shutdown-action proc))

(define/who (custodian-shutdown-all c)
  (check who custodian? c)
  (atomically
   (do-custodian-shutdown-all c))
  ;; Set in "thread.rkt" to check whether the current thread
  ;; should be swapped out
  (post-shutdown-action))

;; Custodians across all places that have a queued shutdown. Hold the
;; memory-limit lock and also disable interrupts (or OK as a GC
;; callback) while modifying this list:
(define queued-shutdowns null)

;; In atomic mode, in an arbitrary host thread but with other threads
;; suspended:
(define (queue-custodian-shutdown! c)
  (unless (custodian-need-shutdown c)
    (set-custodian-need-shutdown! c 'needed)
    (set! queued-shutdowns (cons c queued-shutdowns))
    ;; We can't send a signal to wake up an arbitrary place, because
    ;; the lock on the place is not always taken with interrupts
    ;; disabled. But we don't need a lock to send a wakeup to the
    ;; initial place, because it's wakeup handle never goeas away.
    ;; When the initial place scans for queued shutdowns, it sends
    ;; wakes up to other places as needed.
    (place-wakeup-initial)))

;; Called in atomic mode by the scheduler
(define (check-queued-custodian-shutdown)
  (unless (null? queued-shutdowns)
    (host:disable-interrupts)
    (host:mutex-acquire memory-limit-lock)
    (define queued queued-shutdowns)
    (set! queued-shutdowns
          ;; Keep only custodians owned by other places
          (for/list ([c (in-list queued)]
                     #:unless (custodian-this-place? c))
            (when (eq? (custodian-need-shutdown c) 'needed)
              ;; Make sure custodian's place is polling for shutdowns:
              (set-custodian-need-shutdown! c 'neeed/sent-wakeup)
              (place-wakeup (custodian-place c)))
            c))
    (host:mutex-release memory-limit-lock)    
    (host:enable-interrupts)
    (for ([c (in-list queued)]
          #:when (custodian-this-place? c))
      (do-custodian-shutdown-all c))))

(define place-ensure-wakeup! (lambda () #f)) ; call before enabling shutdowns
(define place-wakeup-initial void)
(define place-wakeup void)
(define (set-place-custodian-procs! ensure-wakeup! wakeup-initial wakeup)
  (set! place-ensure-wakeup! ensure-wakeup!)
  (set! place-wakeup-initial wakeup-initial)
  (set! place-wakeup wakeup))

(define (custodian-this-place? c)
  (eq? (custodian-place c) current-place))

;; In atomic mode
(define (do-custodian-shutdown-all c)
  (unless (custodian-shut-down? c)
    (set-custodian-shut-down?! c #t)
    (for ([(child callback) (in-hash (custodian-children c))])
      (if (procedure-arity-includes? callback 2)
          (callback child c)
          (callback child)))
    (hash-clear! (custodian-children c))
    (let ([sema (custodian-shutdown-sema c)])
      (when sema
        (semaphore-post-all sema)))))

(define (custodian-get-shutdown-sema c)
  (atomically
   (or (custodian-shutdown-sema c)
       (let ([sema (make-semaphore)])
         (set-custodian-shutdown-sema! c sema)
         (when (custodian-shut-down? c)
           (semaphore-post-all sema))
         sema))))

(define (custodian-subordinate? c super-c)
  (let loop ([p-cref (custodian-parent-reference c)])
    (define p (and p-cref (custodian-reference-c p-cref)))
    (cond
      [(eq? p super-c) #t]
      [(not p) #f]
      [else (loop (custodian-parent-reference p))])))

(define (custodian-manages-reference? c cref)
  (define ref-c (custodian-reference-c cref))
  (or (eq? c ref-c)
      (custodian-subordinate? ref-c c)))

(define (custodian-reference->custodian cref)
  (custodian-reference-c cref))

(define/who (custodian-managed-list c super-c)
  (check who custodian? c)
  (check who custodian? super-c)
  (unless (custodian-subordinate? c super-c)
    (raise-arguments-error who "the second custodian does not manage the first custodian"
                           "first custodian" c
                           "second custodian" super-c))
  (hash-keys (custodian-children c)))

(define (custodian-memory-accounting-available?)
  #t)

(define/who (custodian-require-memory limit-cust need-amt stop-cust)
  (check who custodian? limit-cust)
  (check who exact-nonnegative-integer? need-amt)
  (check who custodian? stop-cust)
  (raise (exn:fail:unsupported
          "custodian-require-memory: unsupported"
          (current-continuation-marks))))

(define/who (custodian-limit-memory limit-cust need-amt [stop-cust limit-cust])
  (check who custodian? limit-cust)
  (check who exact-nonnegative-integer? need-amt)
  (check who custodian? stop-cust)
  (place-ensure-wakeup!)
  (atomically/no-interrupts
   (set-custodian-memory-limits! limit-cust
                                 (cons (cons need-amt stop-cust)
                                       (custodian-memory-limits limit-cust)))
   (when (eq? stop-cust limit-cust)
     (define old-limit (custodian-immediate-limit limit-cust))
     (when (or (not old-limit) (old-limit . > . need-amt))
       (set-custodian-immediate-limit! limit-cust need-amt)))
   (host:mutex-acquire memory-limit-lock)
   (set! compute-memory-sizes (max compute-memory-sizes 1))
   (host:mutex-release memory-limit-lock)))

;; ----------------------------------------

(define/who (make-custodian-box c v)
  (check who custodian? c)
  (define b (custodian-box v (custodian-get-shutdown-sema c)))
  (unless (unsafe-custodian-register c b (lambda (b) (set-custodian-box-v! b #f)) #f #t)
    (raise-custodian-is-shut-down who c))
  b)
  
(define/who (custodian-box-value cb)
  (check who custodian-box? cb)
  (custodian-box-v cb))

;; ----------------------------------------

(define (raise-custodian-is-shut-down who c)
  (raise-arguments-error who "the custodian has been shut down"
                         "custodian" c))

;; ----------------------------------------

;; Disable interrupts before taking this lock, since it
;; guards values that are manipulated by a GC callback
(define memory-limit-lock (host:make-mutex))

;; If non-zero, the custodian memory sizes are gathered after a GC.
;; The value decays 
(define compute-memory-sizes 0)

(void (set-reachable-size-increments-callback!
       ;; Called in an arbitary host thread, with interrupts off and all other threads suspended:
       (lambda (compute-size-increments)
         (unless (zero? compute-memory-sizes)
           (host:call-with-current-place-continuation
            (lambda (starting-k)
              ;; Get roots, which are threads and custodians, for all distinct accounting domains
              (define-values (roots custs) ; parallel lists: root and custodian to charge for the root
                (let c-loop ([c initial-place-root-custodian] [pl initial-place] [accum-roots null] [accum-custs null])
                  (set-custodian-memory-use! c 0)
                  (define gc-roots (custodian-gc-roots c))
                  (define roots (if gc-roots
                                    (hash-keys gc-roots)
                                    null))
                  (define host-regs (let ([pl (custodian-place c)])
                                      (if (eq? (place-custodian pl) c)
                                          ;; Charge anything directly reachable from place registers
                                          ;; to the root custodian
                                          (list (place-host-roots pl))
                                          ;; Not the root
                                          null)))
                  (let loop ([roots roots]
                             [local-accum-roots (cons c host-regs)]
                             [accum-roots accum-roots]
                             [accum-custs accum-custs])
                    (cond
                      [(null? roots)
                       (define local-custs (for/list ([root (in-list local-accum-roots)]) c))
                       ;; values owned directly by this custodian need to go earlier in the list,
                       ;; since we're traversing from parent custodian to children
                       (values (append local-accum-roots accum-roots)
                               (append local-custs accum-custs))]
                      [(custodian? (car roots))
                       (define-values (new-roots new-custs) (c-loop (car roots) pl accum-roots accum-custs))
                       (loop (cdr roots) local-accum-roots new-roots new-custs)]
                      [(place? (car roots))
                       (define pl (car roots))
                       (define c (place-custodian pl))
                       (define-values (new-roots new-custs) (c-loop c pl accum-roots accum-custs))
                       (loop (cdr roots) local-accum-roots new-roots new-custs)]
                      [else
                       (define root (car roots))
                       (define new-local-roots (cons root local-accum-roots))
                       (define more-local-roots
                         (cond
                           [(eq? root (place-current-thread pl))
                            (define k-root
                              (if (eq? pl current-place) ; assuming host thread is place main thread
                                  starting-k
                                  (place-host-thread pl)))
                            (cons k-root new-local-roots)]
                           [else new-local-roots]))
                       (loop (cdr roots) more-local-roots accum-roots accum-custs)]))))
              (define sizes (compute-size-increments roots))
              (for ([size (in-list sizes)]
                    [c (in-list custs)])
                (set-custodian-memory-use! c (+ size (custodian-memory-use c))))
              ;; Merge child counts to parents:
              (define any-limits?
                (let c-loop ([c root-custodian])
                  (define gc-roots (custodian-gc-roots c))
                  (define roots (if gc-roots
                                    (hash-keys gc-roots)
                                    null))
                  (define any-limits?
                    (for/fold ([any-limits? #f]) ([root (in-list roots)]
                                                  #:when (custodian? root))
                      (define root-any-limits? (c-loop root))
                      (set-custodian-memory-use! c (+ (custodian-memory-use root)
                                                      (custodian-memory-use c)))
                      (or root-any-limits? any-limits?)))
                  (define use (custodian-memory-use c))
                  (define new-limits
                    (for/list ([limit (in-list (custodian-memory-limits c))]
                               #:when (cond
                                        [((car limit) . <= . use)
                                         (queue-custodian-shutdown! (cdr limit))
                                         #f]
                                        [else #t]))
                      limit))
                  (set-custodian-memory-limits! c new-limits)
                  (or any-limits? (pair? new-limits))))
              ;; If no limits are installed, decay demand for memory counts:
              (unless any-limits?
                (set! compute-memory-sizes (sub1 compute-memory-sizes)))))))))

(void (set-custodian-memory-use-proc!
       ;; Get memory use for a custodian; the second argument is
       ;; total memory use, which is a suitable result for the
       ;; root custodian in the original place.
       (lambda (c all)
         (unless (custodian? c)
           (raise-argument-error 'current-memory-use "(or/c #f 'cumulative custodian?)" c))
         (cond
           [(eq? c root-custodian) all]
           [else
            (when (atomically/no-interrupts
                   (host:mutex-acquire memory-limit-lock)
                   (cond
                     [(zero? compute-memory-sizes)
                      ;; Based on the idea that memory accounting
                      ;; should be about 1/2 the cost of a full GC, so a
                      ;; value of 2 hedges future demands versus
                      ;; no future demands:
                      (set! compute-memory-sizes 2)
                      (host:mutex-release memory-limit-lock)
                      #t]
                     [else
                      (host:mutex-release memory-limit-lock)
                      #f]))
              (collect-garbage))
            (custodian-memory-use c)]))))

(define (custodian-check-immediate-limit mref n)
  (let loop ([mref mref])
    (when mref
      (define c (custodian-reference->custodian mref))
      (when c
        (define limit (custodian-immediate-limit c))
        (when (and limit (n . >= . limit))
          (raise (exn:fail:out-of-memory
                  "out of memory"
                  (current-continuation-marks))))
        (loop (custodian-parent-reference c))))))
