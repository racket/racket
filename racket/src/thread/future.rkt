#lang racket/base
(require "config.rkt"
         "place-local.rkt"
         "place-object.rkt"
         "check.rkt"
         "internal-error.rkt"
         "host.rkt"
         "parameter.rkt"
         "atomic.rkt"
         "custodian-object.rkt"
         "thread.rkt"
         (submod "thread.rkt" for-future)
         (submod "custodian.rkt" for-future)
         "sync.rkt"
         "evt.rkt"
         "future-object.rkt"
         "future-id.rkt"
         "future-lock.rkt"
         "future-logging.rkt"
         "error.rkt")

;; See "README.txt" for some general information about this
;; implementation of futures.

(provide init-future-place!
         futures-enabled?
         future
         future?
         would-be-future
         touch
         future-block
         future-sync
         current-future-prompt
         currently-running-future
         reset-future-logs-for-tracing!
         mark-future-trace-end!
         set-processor-count!)

(module+ for-place
  (provide set-place-future-procs!
           kill-future-scheduler))

(module+ for-fsemaphore
  (provide future*-lock
           set-future*-state!
           future-suspend
           future-notify-dependent
           wakeup-this-place))

(define (init-future-place!)
  (init-future-logging-place!))

(define (futures-enabled?)
  (threaded?))

;; ----------------------------------------

(struct future-evt (future)
  #:property prop:evt (poller (lambda (fe poll-ctx)
                                (define f (future-evt-future fe))
                                (lock-acquire (future*-lock f))
                                (define s (future*-state f))
                                (lock-release (future*-lock f))
                                (cond
                                  [(or (eq? s 'running)
                                       (eq? s 'fsema))
                                   (values #f fe)]
                                  [else (values '(#t) #f)]))))

(define (create-future thunk cust would-be?)
  (define id (get-next-id))
  (log-future 'create #:data id)
  (future* id
           (make-lock)             ; lock
           cust
           would-be?
           thunk
           #f          ; prev
           #f          ; next
           #f          ; results
           #f          ; state
           #hasheq())) ; dependents

(define (future? v)
  (future*? v))

(define future-scheduler-prompt-tag (make-continuation-prompt-tag 'future-scheduler))
(define future-start-prompt-tag (make-continuation-prompt-tag 'future-star))

(define (current-future-prompt)
  (if (current-future)
      future-scheduler-prompt-tag
      (internal-error "not running in a future")))

;; called with lock on f held;
;; in a non-main pthread, caller is responsible for logging 'end-work;
;; in a non-mail thread, decrements `(current-atomic)` just before starting thunk
(define (run-future f #:was-blocked? [was-blocked? #f])
  (set-future*-state! f 'running)
  (define thunk (future*-thunk f))
  (set-future*-thunk! f #f)
  (lock-release (future*-lock f))
  (when was-blocked?
    (when (logging-futures?)
      (log-future 'block (future*-id f) #:prim-name (continuation-current-primitive
                                                     thunk
                                                     '(unsafe-start-atomic)))
      (log-future 'result (future*-id f))))
  (unless (eq? (future*-would-be? f) 'blocked)
    (log-future 'start-work (future*-id f)))
  (define (finish! results state)
    (start-future-uninterrupted)
    (lock-acquire (future*-lock f))
    (set-future*-results! f results)
    (set-future*-state! f state)
    (define deps (future*-dependents f))
    (set-future*-dependents! f #hasheq())
    (lock-release (future*-lock f))
    ;; stay in uninterrupted mode here, because we need to make sure
    ;; that dependents get rescheduled
    (future-notify-dependents deps)
    (end-future-uninterrupted)
    (log-future 'complete (future*-id f)))
  (cond
    [(current-future)
     ;; An attempt to escape will cause the future to block, so
     ;; we only need to handle success
     (call-with-values (lambda ()
                         (call-with-continuation-prompt
                          (lambda ()
                            (current-atomic (sub1 (current-atomic)))
                            (thunk))
                          future-start-prompt-tag
                          (lambda args (void))))
                       (lambda results
                         (finish! results 'done)))]
    [(eq? (future*-would-be? f) #t)
     ;; Similar to `(current-future)` case, but retries
     ;; excplitily if the future blocks
     (call-with-values (lambda ()
                         (call-with-continuation-prompt
                          (lambda ()
                            (current-future f)
                            (begin0
                              (thunk)
                              (current-future #f)))
                          future-start-prompt-tag
                          (lambda args
                            ;; Blocked as a would-be future; `(current-future)` has been
                            ;; reset to #f, and we can retry immediately
                            (set-future*-would-be?! f 'blocked)
                            (touch f))))
                       (lambda results
                         (when (eq? (future*-state f) 'running)
                           (finish! results 'done)
                           (log-future 'end-work (future*-id f)))))]
    [else
     ;; No need for the future prompt tag
     (dynamic-wind
      (lambda () (void))
      (lambda ()
        (with-continuation-mark
         currently-running-future-key f
         (call-with-values thunk
                           (lambda results
                             (finish! results 'done)))))
      (lambda ()
        (unless (eq? (future*-state f) 'done)
          (finish! #f 'aborted))
        (log-future 'end-work (future*-id f))))]))

(define/who (future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (cond
    [(not (futures-enabled?))
     (would-be-future thunk)]
    [else
     (define me-f (current-future))
     (define cust (future-custodian me-f))
     (define f (create-future thunk cust #f))
     (when cust
       (unless me-f
         (maybe-start-scheduler)
         (set-custodian-sync-futures?! cust #t))
       (schedule-future! f))
     f]))

(define/who (would-be-future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (ensure-place-wakeup-handle)
  (create-future thunk (future-custodian (current-future)) #t))

(define (future-custodian me-f)
  (if me-f
      (future*-custodian me-f)
      (thread-representative-custodian (current-thread/in-atomic))))

;; When two futures interact, we may need to adjust both;
;; to keep locks ordered, take lock of future with the
;; lower ID, first; beware that the two futures make be
;; the same (in which case we're headed for a circular
;; dependency)
(define (lock-acquire-both f)
  (define me-f (current-future))
  (cond
    [(or (not me-f)
         (eq? me-f f))
     (lock-acquire (future*-lock f))]
    [((future*-id me-f) . < . (future*-id f))
     (lock-acquire (future*-lock me-f))
     (lock-acquire (future*-lock f))]
    [else
     (lock-acquire (future*-lock f))
     (lock-acquire (future*-lock me-f))]))

(define (lock-release-both f)
  (lock-release-current)
  (lock-release (future*-lock f)))

(define (lock-release-current)
  (define me-f (current-future))
  (when me-f
    (lock-release (future*-lock me-f))))

(define/who (touch f)
  (check who future*? f)
  (lock-acquire-both f)
  (define s (future*-state f))
  (cond
    [(eq? s 'done)
     (lock-release-both f)
     (apply values (future*-results f))]
    [(eq? s 'aborted)
     (lock-release-both f)
     (raise (exn:fail (error-message->string
                       'touch
                       "future previously aborted")
                      (current-continuation-marks)))]
    [(eq? s 'blocked)
     (cond
       [(current-future)
        ;; Can't run a blocked future in a future pthread
        (dependent-on-future f)]
       [else
        ;; Lock on f is held (and no current future to lock)
        (run-future f #:was-blocked? #t)
        (apply values (future*-results f))])]
    [(eq? s #f)
     (cond
       [(current-future)
        ;; Need to wait on `f`, so deschedule current one;
        ;; we may pick `f` next the queue (or maybe later)
        (dependent-on-future f)]
       [(future*-would-be? f) ; => not scheduled
        (lock-release-current)
        ;; Lock on f is held
        (run-future f)
        (apply values (future*-results f))]
       [else
        ;; Give up locks in hope of geting `f` off the
        ;; schedule queue
        (lock-release (future*-lock f))
        (cond
          [(try-deschedule-future? f)
           ;; lock on `f` is held...
           (run-future f)
           (apply values (future*-results f))]
          [else
           ;; Contention, so try again
           (touch f)])])]
    [(eq? s 'running)
     (cond
       [(current-future)
        ;; Stop working on this one until `f` is done
        (dependent-on-future f)]
       [else
        ;; Have to wait until it's not running anywhere
        (set-future*-dependents! f (hash-set (future*-dependents f) 'place #t))
        (lock-release (future*-lock f))
        (log-future 'touch-pause (future*-id f))
        (sync (future-evt f))
        (log-future 'touch-resume (future*-id f))
        (touch f)])]
    [(future? s)
     (cond
       [(current-future)
        ;; Waiting on `s` on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Maybe we can start running `s` to get `f` moving...
        (lock-release (future*-lock f))
        (touch s)
        (touch f)])]
    [(or (box? s) (eq? s 'fsema)) ; => dependent on fsemaphore
     (cond
       [(current-future)
        ;; Lots to wait on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Wait until fsemaphore post succeeds for the future, then try again.
        (lock-release (future*-lock f))
        (log-future 'touch-pause (future*-id f))
        (sync (future-evt f))
        (log-future 'touch-resume (future*-id f))
        (touch f)])]
    [else
     (lock-release (future*-lock f))
     (internal-error "unrecognized future state")]))

;; called in a futurre pthread;
;; called with lock held for both `f` and the current future
(define (dependent-on-future f)
  ;; in a future pthread, so set up a dependency and on `f` and
  ;; bail out, so the current future pthread can do other things;
  ;; note that `me-f` might be the same as `f`, in which case we'll
  ;; create a circular dependency
  (define me-f (current-future))
  (set-future*-dependents! f (hash-set (future*-dependents f) me-f #t))
  (set-future*-state! me-f f)
  (on-transition-to-unfinished)
  (unless (eq? me-f f)
    (lock-release (future*-lock f)))
  ;; almost the same as being blocked, but when `f` completes,
  ;; it will reschedule `me-f`
  (future-suspend f)
  ;; on return from `future-suspend`, no locks are held
  (touch f))

;; called in a future pthread;
;; can be called from Rumble layer
(define (future-block)
  (define me-f (current-future))
  (unless (future*-would-be? me-f)
    (log-future 'block (future*-id me-f)))
  (lock-acquire (future*-lock me-f))
  (set-future*-state! me-f 'blocked)
  (on-transition-to-unfinished)
  (future-suspend))

;; called with lock held on the current future
(define (future-suspend [touching-f #f])
  (define me-f (current-future))
  (call-with-composable-continuation
   (lambda (k)
     (set-future*-thunk! me-f k)
     (lock-release (future*-lock me-f))
     (when touching-f
       (log-future 'touch (future*-id me-f) #:data (future*-id touching-f)))
     (unless (future*-would-be? me-f)
       (log-future 'suspend (future*-id me-f)))
     (cond
       [(future*-would-be? me-f)
        (current-future #f)
        (abort-current-continuation future-start-prompt-tag (void))]
       [else
        (abort-current-continuation future-scheduler-prompt-tag (void))]))
   future-start-prompt-tag))

;; ----------------------------------------

;; Can be in a future thread
;; Call `thunk` in the place's main thread:
(define (future-sync who thunk)
  (define me-f (current-future))
  (cond
    [(future*-would-be? me-f)
     (current-future #f)
     (log-future 'sync (future*-id me-f) #:prim-name who)
     (let ([v (thunk)])
       (log-future 'result (future*-id me-f))
       (current-future me-f)
       v)]
    [else
     ;; In case the main thread is trying to shut down futures, check in:
     (engine-block)
     ;; Host's `call-as-asynchronous-callback` will post `thunk`
     ;; so that it's returned by `host:poll-async-callbacks` to
     ;; the scheduler in the place's main thread; it will also
     ;; tell the scheduler to be in atomic mode so that we don't
     ;; get terminated or swapped out while blocking on the main thread
     (host:call-as-asynchronous-callback
      (lambda ()
        (log-future 'sync (future*-id me-f) #:prim-name who)
        (let ([v (thunk)])
          (log-future 'result (future*-id me-f))
          v)))]))

;; ----------------------------------------

(define pthread-count 1)

;; Called by io layer
(define (set-processor-count! n)
  (set! pthread-count n))

(struct scheduler ([workers #:mutable]
                   [futures-head #:mutable]
                   [futures-tail #:mutable]
                   mutex   ; guards futures chain; see "future-lock.rkt" for discipline
                   cond    ; signaled when chain goes from empty to non-empty
                   ping-cond)
  #:authentic)

(struct worker (id
                [pthread #:mutable]
                current-future-box ; reports current future (for access external to pthread)
                [die? #:mutable]
                [ping #:mutable]) ; box set to #t when the thread should check in with scheduler
  #:authentic)

(define current-scheduler
  (case-lambda
    [() (place-future-scheduler current-place)]
    [(s) (set-place-future-scheduler! current-place s)]))

(define (make-worker id)
  (worker id
          #f         ; pthread
          (box #f)   ; current-future-box
          #f         ; die?
          (box #f)))

;; called in a Racket thread
(define (maybe-start-scheduler)
  (atomically
   (unless (current-scheduler)
     (ensure-place-wakeup-handle)
     (define s (scheduler '()
                          #f  ; futures-head
                          #f  ; futures-tail
                          (host:make-mutex)
                          (host:make-condition)
                          (host:make-condition)))
     (current-scheduler s)
     (define workers
       (for/list ([id (in-range 1 (add1 pthread-count))])
         (define w (make-worker id))
         (start-worker w)
         w))
     (set-scheduler-workers! s workers))))

;; called in atomic mode
(define (kill-future-scheduler)
  (define s (current-scheduler))
  (when s
    (host:mutex-acquire (scheduler-mutex s))
    (for ([w (in-list (scheduler-workers s))])
      (set-worker-die?! w #t))
    (host:mutex-release (scheduler-mutex s))
    (futures-sync-for-shutdown)
    (current-scheduler #f)))

;; called in any pthread
;; called maybe holding an fsemaphore lock, but nothing else
(define (schedule-future! f #:front? [front? #f])
  (define s (current-scheduler))
  (host:mutex-acquire (scheduler-mutex s))
  (define old (if front?
                  (scheduler-futures-head s)
                  (scheduler-futures-tail s)))
  (cond
    [(not old)
     (set-scheduler-futures-head! s f)
     (set-scheduler-futures-tail! s f)]
    [front?
     (set-future*-next! f old)
     (set-future*-prev! old f)
     (set-scheduler-futures-head! s f)]
    [else
     (set-future*-prev! f old)
     (set-future*-next! old f)
     (set-scheduler-futures-tail! s f)])
  (host:condition-signal (scheduler-cond s))
  (host:mutex-release (scheduler-mutex s)))

;; called with queue lock held
(define (deschedule-future f)
  (define s (current-scheduler))
  (cond
    [(or (future*-prev f)
         (future*-next f))
     (if (future*-prev f)
         (set-future*-next! (future*-prev f) (future*-next f))
         (set-scheduler-futures-head! s (future*-next f)))
     (if (future*-next f)
         (set-future*-prev! (future*-next f) (future*-prev f))
         (set-scheduler-futures-tail! s (future*-prev f)))
     (set-future*-prev! f #f)
     (set-future*-next! f #f)]
    [(eq? f (scheduler-futures-head s))
     (set-scheduler-futures-head! s #f)
     (set-scheduler-futures-tail! s #f)]
    [else
     (internal-error "future is not in queue")]))

;; called with no locks held; if successful,
;; returns with lock held on f
(define (try-deschedule-future? f)
  (define s (current-scheduler))
  (host:mutex-acquire (scheduler-mutex s))
  (define ok?
    (cond
      [(and (not (future*-prev f))
            (not (future*-next f))
            (not (eq? f (scheduler-futures-head s))))
       ;; Was descheduled by someone else already, or maybe
       ;; hasn't yet made it back into the schedule after a
       ;; dependency triggered `future-notify-dependent`
       #f]
      [else
       (deschedule-future f)
       (lock-acquire (future*-lock f))
       #t]))
  (host:mutex-release (scheduler-mutex s))
  ok?)

;; called in any pthread
;; called maybe holding an fsemaphore lock, but nothing else
(define (future-notify-dependents deps)
  (for ([f (in-hash-keys deps)])
    (cond
      [(eq? f 'place) (wakeup-this-place)]
      [else (future-notify-dependent f)])))

;; called in any pthread
;; called maybe holding an fsemaphore lock, but nothing else
(define (future-notify-dependent f)
  (with-lock (future*-lock f)
    (set-future*-state! f #f))
  (on-transition-to-unfinished)
  (if (future*-would-be? f)
      (wakeup-this-place)
      (schedule-future! f #:front? #t)))

;; ----------------------------------------

(define-syntax-rule (keep-trying e)
  (let loop () (unless e (loop))))

(define (start-worker w)
  (define s (current-scheduler))
  (define th
    (fork-pthread
     (lambda ()
       (current-future 'worker)
       (host:mutex-acquire (scheduler-mutex s))
       (let loop ()
         (check-in w)
         (cond
           [(worker-die? w) ; worker was killed
            (host:mutex-release (scheduler-mutex s))]
           [(scheduler-futures-head s)
            => (lambda (f)
                 (deschedule-future f)
                 (host:mutex-release (scheduler-mutex s))
                 (lock-acquire (future*-lock f))
                 ;; lock is held on f; run the future
                 (maybe-run-future-in-worker f w)
                 ;; look for more work
                 (host:mutex-acquire (scheduler-mutex s))
                 (loop))]
           [else
            ;; wait for work
            (host:condition-wait (scheduler-cond s) (scheduler-mutex s))
            (loop)])))))
  (set-worker-pthread! w th))

;; called with lock on f
(define (maybe-run-future-in-worker f w)
  ;; Don't start the future if the custodian is shut down,
  ;; because we may have transitioned from 'pending to
  ;; 'running without an intervening check
  (cond
    [(custodian-shut-down?/other-pthread (future*-custodian f))
     (set-future*-state! f 'blocked)
     (on-transition-to-unfinished)
     (lock-release (future*-lock f))]
    [else
     (run-future-in-worker f w)]))

(define (run-future-in-worker f w)
  (current-future f)
  (set-box! (worker-current-future-box w) f)
  ;; If we didn't need to check custodians, could be just
  ;;   (call-with-continuation-prompt
  ;;     (lambda () (run-future f))
  ;;     future-scheduler-prompt-tag
  ;;     void)
  ;; But use an engine so we can periodically check that the future is
  ;; still supposed to run.
  ;; We take advantage of `current-atomic`, which would otherwise
  ;; be unused, to disable interruptions.
  (define e (make-engine (lambda ()
                           ;; decrements `(current-atomic)`
                           (run-future f))
                         future-scheduler-prompt-tag
                         void
                         break-enabled-default-cell
                         #t))
  (current-atomic (add1 (current-atomic)))
  (call-with-engine-completion
   (lambda (done)
     (let loop ([e e])
       (e TICKS
          (lambda ()
            ;; Check whether the main pthread wants to know we're here
            (when (and (zero? (current-atomic))
                       (worker-pinged? w))
              (host:mutex-acquire (scheduler-mutex (current-scheduler)))
              (check-in w)
              (host:mutex-release (scheduler-mutex (current-scheduler))))
            ;; Check that the future should still run
            (when (and (or (custodian-shut-down?/other-pthread (future*-custodian f))
                           (worker-die? w))
                       (zero? (current-atomic)))
              (lock-acquire (future*-lock f))
              (set-future*-state! f #f)
              (on-transition-to-unfinished)
              (future-suspend)))
          (lambda (e results leftover-ticks)
            (cond
              [e (loop e)]
              [else
               ;; Done --- completed or suspend (e.g., blocked)
               (done (void))]))))))
  (log-future 'end-work (future*-id f))
  (current-future 'worker)
  (set-box! (worker-current-future-box w) #f))

;; in atomic mode
(define (futures-sync-for-shutdown)
  ;; Make sure any futures that are running in a future pthread
  ;; have had a chance to notice a custodian shutdown or a
  ;; future-scheduler shutdown.
  ;;
  ;; Assert: all workers have `ping` as #f.
  (define s (current-scheduler))
  (host:mutex-acquire (scheduler-mutex s))
  (for ([w (in-list (scheduler-workers s))])
    (let retry ()
      (unless (box-cas! (worker-ping w) #f #t)
        (retry))))
  ;; Assert: all workers have `ping` as #t.
  ;; Wake up idle threads so they check in:
  (host:condition-broadcast (scheduler-cond s))
  (drain-async-callbacks (scheduler-mutex s)) ; releases and re-acquires mutex
  ;; When a worker sets `ping` to #f, they must broadcast
  ;; a wakeup for the following loop's benefit
  (let loop ()
    (when (for/or ([w (in-list (scheduler-workers s))])
            (unbox (worker-ping w)))
      (host:condition-wait (scheduler-ping-cond s) (scheduler-mutex s))
      (loop)))
  ;; Assert: all workers have `ping` as #f.
  (host:mutex-release (scheduler-mutex s)))

;; lock-free synchronization to check whether the box content is #f
(define (worker-pinged? w)
  (cond
    [(box-cas! (worker-ping w) #t #t) #t]
    [(box-cas! (worker-ping w) #f #f) #f]
    [else (worker-pinged? w)]))

;; called with scheduler lock
(define (check-in w)
  (when (unbox (worker-ping w))
    (set-box! (worker-ping w) #f)
    (host:condition-broadcast (scheduler-ping-cond (current-scheduler)))))

;; in atomic mode
;; While we're trying to finish up futures, some of them
;; may be blocked waiting for async callbacks. No new ones
;; will get posted since we've set the ping flag, so we
;; only have to drain once.
(define (drain-async-callbacks m)
  (host:mutex-release m)
  (define callbacks (host:poll-async-callbacks))
  (for ([callback (in-list callbacks)])
    (callback))
  (host:mutex-acquire m))

;; ----------------------------------------

;; called in a GCing pthread with all other pthreads stopped
(define (scheduler-add-thread-custodian-mapping! s ht)
  (when s
    (for ([w (in-list (scheduler-workers s))])
      (define f (unbox (worker-current-future-box w)))
      (when f
        (define c (future*-custodian f))
        (when c
          (hash-set! ht c (cons (worker-pthread w)
                                (hash-ref ht c null))))))))

;; ----------------------------------------

(define (reset-future-logs-for-tracing!)
  (atomically
   (flush-future-log)))

(define (mark-future-trace-end!)
  (log-future 'stop-trace #f))

;; ----------------------------------------

;; When a future changes from a state where the main thread may be
;; waiting for it, then make sure there's a wakeup signal
(define (on-transition-to-unfinished)
  (define me-f (current-future))
  (when (and me-f
             (not (future*-would-be? me-f)))
    (wakeup-this-place)))

(define wakeup-this-place (lambda () (void)))
(define ensure-place-wakeup-handle (lambda () (void)))

(define (set-place-future-procs! wakeup ensure)
  (set! wakeup-this-place wakeup)
  (set! ensure-place-wakeup-handle ensure))

;; tell "atomic.rkt" layer how to block:
(void (set-future-block! future-block))

;; tell "custodian.rkt" how to sync and map pthreads to custodians:
(void (set-custodian-future-callbacks! futures-sync-for-shutdown
                                       scheduler-add-thread-custodian-mapping!))
