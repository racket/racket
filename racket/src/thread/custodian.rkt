#lang racket/base
(require "custodian-object.rkt"
         "place-object.rkt"
         "place-local.rkt"
         "check.rkt"
         "atomic.rkt"
         "host.rkt"
         "evt.rkt"
         "semaphore.rkt"
         "parameter.rkt"
         "sink.rkt"
         "exit.rkt")

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
           create-custodian
           poll-custodian-will-executor))

(module+ for-future
  (provide set-custodian-future-callbacks!))

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
;; the managed objects into the parent. To support multiple moves,
;; `c` can be another reference
(struct custodian-reference ([c #:mutable])
  #:authentic)

(define-place-local custodian-will-executor (host:make-late-will-executor void #f))

(define/who current-custodian
  (make-parameter root-custodian
                  (lambda (v)
                    (check who custodian? v)
                    v)))

;; To initialize a new place:
(define (set-root-custodian! c)
  (set! root-custodian c)
  (current-custodian c)
  (set! custodian-will-executor (host:make-late-will-executor void #f)))

(define/who (make-custodian [parent (current-custodian)])
  (check who custodian? parent)
  (define c (create-custodian parent))
  (set-custodian-place! c (custodian-place parent))
  (define cref (do-custodian-register parent c
                                      ;; Retain children procs as long as proc for `c`
                                      (let ([children (custodian-children c)])
                                        (lambda (c)
                                          (reference-sink children)
                                          (do-custodian-shutdown-all c)))
                                      #:at-exit? #t
                                      #:gc-root? #t))
  (set-custodian-parent-reference! c cref)
  (unless cref (raise-custodian-is-shut-down who parent))
  (host:will-register custodian-will-executor c merge-custodian-into-parent)
  c)

(define (unsafe-make-custodian-at-root)
  (make-custodian root-custodian))

;; The given `callback` will be run in atomic mode.
;; Unless `weak?` is true, the given `obj` is registered with an ordered
;; finalizer; in that case, if `obj` is exposed to safe code, it can
;; have its own finalizers, but weak boxes or hashtable references will
;; not be cleared until the value is explicitly shut down.
(define (do-custodian-register cust obj callback
                               #:at-exit? [at-exit? #f]
                               #:weak? [weak? #f]
                               #:gc-root? [gc-root? #f])
  (atomically
   (cond
     [(custodian-shut-down? cust) #f]
     [else
      (define we (and (not weak?)
                      (host:make-will-executor void)))
      (hash-set! (custodian-children cust)
                 obj
                 (cond
                   [weak? callback]
                   [at-exit? (at-exit-callback callback we)]
                   [else (willed-callback callback we)]))
      (when we
        ;; Registering with a will executor that we retain but never
        ;; poll has the effect of turning a semi-weak reference
        ;; (allows other finalizers, but doesn't clear weak boxes)
        ;; into a strong one when there are no other references;
        ;; we're assuming that no wills were previously registered
        ;; so that this one is last on the stack of wills:
        (host:will-register we obj void))
      (when gc-root?
        (host:disable-interrupts)
        (unless (custodian-gc-roots cust)
          (set-custodian-gc-roots! cust (make-weak-hasheq)))
        (hash-set! (custodian-gc-roots cust) obj #t)
        (host:enable-interrupts))
      (or (custodian-self-reference cust)
          (let ([cref (custodian-reference cust)])
            (set-custodian-self-reference! cust cref)
            cref))])))

(define (unsafe-custodian-register cust obj callback at-exit? weak?)
  (do-custodian-register cust obj callback #:at-exit? at-exit? #:weak? weak?))

(define (custodian-register-thread cust obj callback)
  (do-custodian-register cust obj callback #:weak? #t #:gc-root? #t))

(define (custodian-register-place cust obj callback)
  (do-custodian-register cust obj callback #:weak? #t #:gc-root? #t))

(define (unsafe-custodian-unregister obj cref)
  (when cref
    (atomically
     (define c (custodian-reference->custodian cref))
     (unless (custodian-shut-down? c)
       (hash-remove! (custodian-children c) obj))
     (host:disable-interrupts)
     (define gc-roots (custodian-gc-roots c))
     (when gc-roots
       (hash-remove! gc-roots obj))
     (host:enable-interrupts))
    (void)))

;; Called by scheduler (so atomic) when `c` is unreachable
(define (merge-custodian-into-parent c)
  (unless (custodian-shut-down? c)
    (define p-cref (custodian-parent-reference c))
    (define parent (custodian-reference->custodian p-cref))
    (define gc-roots (custodian-gc-roots c))
    (unsafe-custodian-unregister c p-cref)
    (for ([(child callback) (in-hash (custodian-children c))])
      (define gc-root? (and gc-roots (hash-ref gc-roots child #f) #t))
      (cond
        [(willed-callback? callback)
         (do-custodian-register parent child (willed-callback-proc callback)
                                #:at-exit? (at-exit-callback? callback)
                                #:gc-root? gc-root?)]
        [else
         (do-custodian-register parent child callback
                                #:gc-root? gc-root?)]))
    (define self-ref (custodian-self-reference c))
    (when self-ref
      (set-custodian-reference-c! self-ref (custodian-self-reference parent)))
    (hash-clear! (custodian-children c))
    (when gc-roots (hash-clear! gc-roots))))
  
;; Called in scheduler thread:
(define (poll-custodian-will-executor)
  (cond
    [(host:will-try-execute custodian-will-executor)
     => (lambda (p)
          ((car p) (cdr p))
          (poll-custodian-will-executor))]))

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
  (cond
    [(null? queued-shutdowns) #f]
    [else
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
       (do-custodian-shutdown-all c))
     #t]))

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
    (when (custodian-sync-futures? c)
      (futures-sync-for-custodian-shutdown))
    (for ([(child callback) (in-hash (custodian-children c))])
      (if (procedure-arity-includes? callback 2)
          (callback child c)
          (callback child)))
    (hash-clear! (custodian-children c))
    (let ([sema (custodian-shutdown-sema c)])
      (when sema
        (semaphore-post-all sema)))
    (define p-cref (custodian-parent-reference c))
    (when p-cref
      (unsafe-custodian-unregister c p-cref))
    (hash-remove! custodians-with-limits c)))

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
    (define p (and p-cref (custodian-reference->custodian p-cref)))
    (cond
      [(eq? p super-c) #t]
      [(not p) #f]
      [else (loop (custodian-parent-reference p))])))

(define (custodian-manages-reference? c cref)
  (define ref-c (custodian-reference->custodian cref))
  (or (eq? c ref-c)
      (custodian-subordinate? ref-c c)))

(define (custodian-reference->custodian cref)
  (define c (custodian-reference-c cref))
  (cond
    [(custodian-reference? c)
     (define next-c (custodian-reference-c c))
     (cond
       [(custodian-reference? next-c)
        ;; shrink the chain
        (set-custodian-reference-c! cref next-c)
        (custodian-reference->custodian cref)]
       [else next-c])]
    [else c]))

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
   (hash-set! custodians-with-limits limit-cust #t)
   (host:mutex-acquire memory-limit-lock)
   (set! compute-memory-sizes (max compute-memory-sizes 1))
   (host:mutex-release memory-limit-lock)))

;; Ensures that custodians with memory limits are not treated as
;; inaccessible and merged:
(define custodians-with-limits (make-hasheq))

;; ----------------------------------------

(define/who (make-custodian-box c v)
  (check who custodian? c)
  (define b (custodian-box v (custodian-get-shutdown-sema c)))
  (unless (do-custodian-register c b (lambda (b) (set-custodian-box-v! b #f)) #:weak? #t #:gc-root? #t)
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

(define futures-sync-for-custodian-shutdown (lambda () (void)))
(define future-scheduler-add-thread-custodian-mapping! (lambda (s ht) (void)))

(define (set-custodian-future-callbacks! sync-shutdown add-custodian-mapping)
  (set! futures-sync-for-custodian-shutdown sync-shutdown)
  (set! future-scheduler-add-thread-custodian-mapping! add-custodian-mapping))

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
              ;; A place may have future pthreads, and each ptherad may
              ;; be running a future that becomes to a particular custodian;
              ;; build up a custodian-to-pthtread mapping in this table:
              (define custodian-future-threads (make-hasheq))
              (future-scheduler-add-thread-custodian-mapping! (place-future-scheduler initial-place)
                                                              custodian-future-threads)
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
                       (future-scheduler-add-thread-custodian-mapping! (place-future-scheduler pl)
                                                                       custodian-future-threads)
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
                (let c-loop ([c initial-place-root-custodian])
                  (define gc-roots (custodian-gc-roots c))
                  (define roots (append
                                 (hash-ref custodian-future-threads c null)
                                 (if gc-roots
                                     (hash-keys gc-roots)
                                     null)))
                  (define any-limits?
                    (for/fold ([any-limits? #f]) ([root (in-list roots)]
                                                  #:when (or (custodian? root)
                                                             (place? root)))
                      (define next-c (if (custodian? root)
                                         root
                                         (place-custodian root)))
                      (define root-any-limits? (c-loop next-c))
                      (set-custodian-memory-use! c (+ (custodian-memory-use next-c)
                                                      (custodian-memory-use c)))
                      (or root-any-limits? any-limits?)))
                  (define use (custodian-memory-use c))
                  (define old-limits (custodian-memory-limits c))
                  (define new-limits
                    (for/list ([limit (in-list old-limits)]
                               #:when (cond
                                        [((car limit) . <= . use)
                                         (queue-custodian-shutdown! (cdr limit))
                                         #f]
                                        [else #t]))
                      limit))
                  (set-custodian-memory-limits! c new-limits)
                  (when (and (pair? old-limits)
                             (null? new-limits))
                    (hash-remove! custodians-with-limits c))
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
           [(eq? c initial-place-root-custodian) all]
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
