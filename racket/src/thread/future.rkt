#lang racket/base
(require racket/fixnum
         "config.rkt"
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
         (submod "semaphore.rkt" for-future)
         "sync.rkt"
         "evt.rkt"
         "future-object.rkt"
         "future-id.rkt"
         "future-lock.rkt"
         "future-logging.rkt"
         "error.rkt"
         (only-in '#%paramz
                  parameterization-key)
         (only-in '#%unsafe
                  unsafe-call-with-composable-continuation/no-wind
                  unsafe-abort-current-continuation/no-wind))

;; See "README.txt" for some general information about this
;; implementation of futures.

(provide init-future-place!
         futures-enabled?
         future
         future?
         would-be-future
         touch
         thread/parallel
         make-parallel-thread-pool
         parallel-thread-pool-close
         parallel-thread-pool?
         future-block
         current-future-prompt
         currently-running-future
         reset-future-logs-for-tracing!
         mark-future-trace-end!
         set-processor-count!)

(module+ for-place
  (provide set-place-future-procs!
           kill-future-schedulers))

(module+ for-fsemaphore
  (provide future*-lock
           set-future*-state!
           future-maybe-notify-stop
           future-suspend
           future-notify-dependent
           wakeup-this-place
           future-unblock
           call-in-future))

(define (init-future-place!)
  (init-future-logging-place!))

(define (futures-enabled?)
  (threaded?))

;; ----------------------------------------

(struct future-evt (future wait-done?)
  #:property prop:evt (poller (lambda (fe poll-ctx)
                                (define f (future-evt-future fe))
                                (lock-acquire (future*-lock f))
                                (define s (future*-state f))
                                (lock-release (future*-lock f))
                                (cond
                                  [(if (future-evt-wait-done? fe)
                                       (not (eq? s 'done))
                                       (or (eq? s 'running)
                                           (eq? s 'fsema)))
                                   (values #f fe)]
                                  [else (values '(#t) #f)]))))

(define-place-local fsemaphore-wait-poll #f)

(define (create-future thunk cust would-be?)
  (define id (get-next-id))
  (log-future 'create #:data id)
  (future* id
           (make-lock) ; lock
           cust
           #f          ; parallel
           (and would-be? 'would-be)
           thunk
           #f          ; prev
           #f          ; next
           #f          ; results
           #f          ; state
           #hasheq()   ; dependents
           #f          ; suspend-pthread-id
           #f))        ; suspend-timestamp

(define (future? v)
  (future*? v))

(define (current-future-in-future-thread) ; includes would-be futures
  (define f (current-future))
  (and f
       (or (in-future-thread?)
           (eq? (future*-kind f) 'would-be))
       f))

(define (current-parallel-future-in-racket-thread)
  (define f (current-future))
  (and f
       (let ([t (current-thread/in-racket)])
         (and t (future*-parallel f)))
       f))

(define future-scheduler-prompt-tag (make-continuation-prompt-tag 'future-scheduler))
(define future-start-prompt-tag (make-continuation-prompt-tag 'future-start))

(define (current-future-prompt)
  (define f (current-future))
  (cond
    [(not f)
     ;; can happen just after changing from 'would-be to 'was if something
     ;; prompt-related precipitated the block
     #f]
    [(future*-parallel f)
     ;; in a parallel thread, the future sees the full continuation,
     ;; whether it's running in a future pthread or as a Racket thread
     #f]
    [else
     future-scheduler-prompt-tag]))

;; called with lock on f held;
;; in a non-main pthread, caller is responsible for logging 'end-work;
;; in a non-mail thread, calls `(end-uninterruptible)` just before starting thunk
(define (run-future f
                    #:was-blocked? [was-blocked? #f]
                    #:as-unblock? [as-unblock? #f])
  (assert (or (not (future*-state f))
              (and (eq? (future*-state f) 'blocked)
                   (in-racket-thread?))))
  (assert (not (future-scheduled? f)))
  (set-future*-state! f (if as-unblock?
                            #f ; like an unscheduled future
                            'running))
  (define thunk (future*-thunk f))
  (assert thunk)
  (set-future*-thunk! f #f)
  (lock-release (future*-lock f))
  (when was-blocked?
    (when (logging-futures?)
      (define prim-name (continuation-current-primitive* thunk))
      (log-future (if as-unblock? 'sync 'block) (future*-id f)
                  #:timestamp (if (future*-kind f)
                                  (current-inexact-milliseconds)
                                  (future*-suspend-timestamp f))
                  #:pthread-id (and (not (future*-kind f))
                                    (future*-suspend-pthread-id f))
                  #:prim-name prim-name)
      (log-future (if as-unblock? 'sync 'result) (future*-id f)
                  #:prim-name prim-name)))
  (unless (eq? (future*-kind f) 'was)
    (log-future 'start-work (future*-id f)))
  (define (finish! results state)
    (start-uninterruptible)
    (lock-acquire (future*-lock f))
    (future-maybe-notify-stop f)
    (set-future*-results! f results)
    (set-future*-state! f state)
    (define deps (future*-dependents f))
    (set-future*-dependents! f #hasheq())
    (lock-release (future*-lock f))
    ;; stay in uninterrupted mode here, because we need to make sure
    ;; that dependents get rescheduled
    (future-notify-dependents deps)
    (wakeup-racket-thread f)
    (end-uninterruptible)
    (log-future 'complete (future*-id f)))
  (cond
    [(current-future-in-future-thread)
     (define p (future*-parallel f))
     (when p
       (set-engine-thread-cell-state! (parallel*-cells p)))
     ;; An attempt to escape will cause the future to block, so
     ;; we only need to handle success
     (call-with-values (lambda ()
                         (call-with-continuation-prompt
                          (lambda ()
                            (end-uninterruptible) ; balances start in `run-future-in-worker`
                            (thunk))
                          future-start-prompt-tag
                          (lambda args (void))))
                       (lambda results
                         (finish! results 'done)))]
    [as-unblock?
     ;; result is ignored, and will not block, but might suspend
     ;; to be rescheduled to run in a future pthread
     (current-future f)
     (set-engine-thread-cell-state! (parallel*-cells (future*-parallel f)))
     ;; unblock thread's start has `future-start-prompt-tag` prompt:
     (thunk)]
    [(and (eq? (future*-kind f) 'would-be)
          ;; if we're in a parallel thread, don't try to track would-be blocking
          (not (current-future)))
     ;; Similar to `(current-future-in-future-thread)` case, but retries
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
                            ;; reset to #f, and `future-kind` set to 'was, and we can retry immediately
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
     (when (and me-f (future*-parallel me-f)) (future-block)) ; note: don't block plain futures
     (define cust (future-custodian me-f))
     (when (and cust (not me-f))
       (maybe-start-scheduler)
       (set-custodian-sync-futures?! cust #t))
     (when (and me-f (future*-parallel me-f)) (future-unblock))
     (define f (create-future thunk cust #f))
     (when cust
       (schedule-future! f))
     f]))

(define/who (would-be-future thunk)
  (check who (procedure-arity-includes/c 0) thunk)
  (define me-f (current-future))
  (when (and me-f (future*-parallel me-f)) (future-block)) ; note: don't block plain futures
  (ensure-place-wakeup-handle)
  (define cust (future-custodian (current-future)))
  (when (and me-f (future*-parallel me-f)) (future-unblock))
  (define f (create-future thunk cust #t))
  f)

;; not allowed in a future pthread for a parallel thread
(define (future-custodian me-f)
  (assert (or (not (current-future)) (not (future*-parallel (current-future)))))
  (if me-f
      (future*-custodian me-f)
      (thread-representative-custodian (current-thread/in-racket))))

(define (custodian-shut-down?/other-pthread* c)
  ;; parallel thread futures have no custodian of their own
  (and c (custodian-shut-down?/other-pthread c)))

(define/who (make-parallel-thread-pool [n pthread-count])
  (check who exact-positive-integer? n)
  (make-phantom-bytes (* n 1024)) ; intended to make sure that `n` is reasonable
  (create-parallel-thread-pool 'make-parallel-thread-pool n +inf.0 (current-custodian) #f))

(define (create-parallel-thread-pool who n capacity cust own?)
  (or (atomically
       (define s (start-scheduler n #t))
       (set-place-schedulers! current-place (hash-set (place-schedulers current-place) s #t))
       (define pool (parallel-thread-pool s capacity 0 #f))
       (define (close pool)
         (define s (parallel-thread-pool-scheduler pool))
         (define schedulers (place-schedulers current-place))
         (when (hash-ref schedulers s #f)
           (kill-future-scheduler s)
           (set-place-schedulers! current-place (hash-remove schedulers s)))
         (unsafe-custodian-unregister pool (parallel-thread-pool-custodian-reference pool)))
       (cond
         [own?
          ;; leave custodian shutdown to thread, but register a will in case
          ;; the thread is GCed
          (host:will-register custodian-will-executor pool close)
          pool]
         [(not cust)
          pool]
         [else
          (define cref (custodian-register-pool cust pool (lambda (pool c) (close pool))))
          (set-parallel-thread-pool-custodian-reference! pool cref)
          (and cref pool)]))
      (raise-custodian-is-shut-down who cust)))

(define/who (parallel-thread-pool-close pool)
  (check who parallel-thread-pool? pool)
  (define s (parallel-thread-pool-scheduler pool))
  (host:mutex-acquire (scheduler-mutex s))
  (set-parallel-thread-pool-capacity! pool 0)
  (host:mutex-release (scheduler-mutex s))
  (atomically
   (thread-pool-departure pool 0)))

(define (thread/parallel thunk [pool-in 'own] [keep-result? #f])
  (define who 'thread)
  (check who (procedure-arity-includes/c 0) thunk)
  (check who (lambda (v) (or (eq? v 'own) (parallel-thread-pool? v)))
         #:contract "(or/c #f 'own parallel-thread-pool?)"
         pool-in)
  (check who (lambda (v) (or (not v) (eq? v 'results))) #:contract "(or/c #f 'results)" keep-result?)
  (cond
    [(not (futures-enabled?))
     (thread thunk)]
    [else
     (define cust (current-custodian))
     (define pool (if (eq? pool-in 'own)
                      (create-parallel-thread-pool 'thread 1 1 cust #t)
                      pool-in))
     (define paramz (current-parameterization))
     (define break-enabled (current-break-enabled-cell))
     (define thunk-in-prompt
       (lambda ()
         ;; Use the default prompt tag inside a prompt with
         ;; `future-start-prompt-tag` so that capturing the
         ;; continuation (as far as clients can tell) does
         ;; not capture its futureness, and also so that
         ;; continuation-mark actions generally complete local
         ;; to the future
         (call-with-continuation-prompt
          (lambda ()
            (with-continuation-mark
              parameterization-key paramz
              (with-continuation-mark
                break-enabled-key break-enabled
                (begin
                  (check-for-break)
                  (if keep-result?
                      (thunk)
                      (begin
                        (thunk)
                        (void)))))))
          (default-continuation-prompt-tag)
          no-results-on-abort-handler)))
     (define me-f (create-future thunk-in-prompt #f #f))
     (define-values (th cells)
       (do-make-thread who
                       #:name (object-name thunk)
                       #:break-enabled-cell parallel-break-disabled-cell
                       #:custodian cust
                       #:schedule? #f
                       #:keep-result? keep-result?
                       #:return-cells? #t
                       (lambda ()
                         (let loop ()
                           (call-with-continuation-prompt
                            (lambda () (touch-blocked me-f))
                            future-start-prompt-tag
                            (lambda args
                              (loop)))))))
     (set-future*-parallel! me-f (parallel* pool th #f cells))
     (thread-init-kill-callback! th (lambda ()
                                      (future-external-stop me-f)
                                      (thread-pool-departure pool -1)))
     (thread-push-suspend+resume-callbacks! (lambda () (future-external-stop me-f))
                                            (lambda () (future-external-resume me-f))
                                            th)
     ;; this is the step (internally atomic) that commits the thread to running:
     (schedule-future! me-f #:check-pool-open? #t)
     th]))

;; in Racket thread
;; used to implement `fsemaphore-wait` when not in a future
(define (call-in-future thunk)
  (define me-f
    (cond
      [(futures-enabled?)
       (define me-f (create-future thunk #f #f))
       (define pool
         (atomically
          (or fsemaphore-wait-poll
              (let ([pool (create-parallel-thread-pool 'call-in-future 1 +inf.0 #f #f)])
                (set! fsemaphore-wait-poll pool)
                pool))))
       (set-future*-parallel! me-f (parallel* pool #f #f #f))
       me-f]
      [else (would-be-future thunk)]))
  (dynamic-wind
    (lambda ()
      (atomically
       (thread-push-kill-callback! (lambda () (future-external-stop me-f)))
       (thread-push-suspend+resume-callbacks! (lambda () (future-external-stop me-f))
                                              (lambda () (future-external-resume me-f)))))
    (lambda ()
      (when (futures-enabled?)
        (schedule-future! me-f))
      (sync (future-evt me-f #t)))
    (lambda ()
      (atomically
       (future-external-stop me-f)
       (thread-pop-suspend+resume-callbacks!)
       (thread-pop-kill-callback!)))))

;; When two futures interact, we may need to adjust both;
;; to keep locks ordered, take lock of future with the
;; lower ID, first; beware that the two futures may be
;; the same (in which case we're headed for a circular
;; dependency)
(define (lock-acquire-both f)
  (define me-f (current-future-in-future-thread))
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
  (define me-f (current-future-in-future-thread))
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
    [(let ([cf (current-future)])
       (and cf (future*-parallel cf)))
     ;; We're in a `thread/parallel` future pthread, but since `f` is
     ;; some other future, it must be one that can be referenced directly,
     ;; and it's managed by the scheduler for `future`-created futures;
     ;; block so that the future is handled in a Racket thread
     (future-block)
     (begin0
       (touch f)
       (future-barrier-exit))]
    [(eq? s 'blocked)
     (cond
       [(current-future-in-future-thread)
        ;; Can't run a blocked future in a future pthread
        (dependent-on-future f)]
       [else
        ;; Lock on f is held (and no current future to lock)
        (run-future f #:was-blocked? #t)
        (apply values (future*-results f))])]
    [(eq? s #f)
     (cond
       [(current-future-in-future-thread)
        ;; Need to wait on `f`, so deschedule current one;
        ;; we may pick `f` next the queue (or maybe later)
        (dependent-on-future f)]
       [(future*-kind f) ; => not scheduled
        (lock-release-current)
        ;; Lock on f is held
        (run-future f)
        (apply values (future*-results f))]
       [else
        ;; Can't be a parallel thread future, so since it isn't blocked,
        ;; it must be scheduled or has a shut down custodian
        (try-deschedule-future? f)
        (run-future f)
        (apply values (future*-results f))])]
    [(eq? s 'running)
     (cond
       [(current-future-in-future-thread)
        ;; Stop working on this one until `f` is done
        (dependent-on-future f)]
       [else
        ;; Have to wait until it's not running anywhere
        (set-future*-dependents! f (hash-set (future*-dependents f) 'place #t))
        (lock-release (future*-lock f))
        (log-future 'touch-pause (future*-id f))
        (sync (future-evt f #f))
        (log-future 'touch-resume (future*-id f))
        (touch f)])]
    [(future? s)
     (cond
       [(current-future-in-future-thread)
        ;; Waiting on `s` on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Maybe we can start running `s` to get `f` moving...
        (lock-release (future*-lock f))
        (touch s)
        (touch f)])]
    [(or (box? s) (eq? s 'fsema)) ; => dependent on fsemaphore
     (cond
       [(current-future-in-future-thread)
        ;; Lots to wait on, so give up on the current future for now
        (dependent-on-future f)]
       [else
        ;; Wait until fsemaphore post succeeds for the future, then try again.
        (lock-release (future*-lock f))
        (log-future 'touch-pause (future*-id f))
        (sync (future-evt f #f))
        (log-future 'touch-resume (future*-id f))
        (touch f)])]
    [else
     (lock-release (future*-lock f))
     (internal-error "unrecognized future state")]))

;; called in a Racket thread for a parallel thread to start running `f`,
;; which has gotten blocked in its future pthread
(define/who (touch-blocked f)
  (lock-acquire (future*-lock f))
  (define s (future*-state f))
  (cond
    [(eq? s 'blocked)
     (run-future f #:was-blocked? #t #:as-unblock? #t)]
    [(eq? s 'done)
     (lock-release (future*-lock f))
     (apply values (future*-results f))]
    [(eq? s 'aborted)
     (lock-release (future*-lock f))
     #f]
    [else
     ;; the future is not blocked; suspend and get resumed if/when
     ;; needed again; make sure we deschedule while still in atomic
     ;; mode (via the future locks' uninterruptible mode), so that
     ;; the thread is definitely descheduled and can be reshceduled by
     ;; the future via `wakeup-racket-thread`
     ((thread-deschedule! #:last-step (lambda () (lock-release (future*-lock f)))
                          (current-thread/in-racket) #f 'future))
     ;; only reason we should get rescheduled is `wakeup-racket-thread`
     ;; or (if that one is skipped by a stop request) `future-external-resume`
     (touch-blocked f)]))

;; called in a future pthread;
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

;; called in a future pthread, in a Racket thread running a would-be future,
;; or in a Racket thread is that is a future's unblock thread;
;; can be called from Rumble layer
(define (future-block)
  (start-uninterruptible)
  (define me-f (current-future-in-future-thread))
  (cond
    [me-f
     (lock-acquire (future*-lock me-f))
     (end-uninterruptible) ; future lock covers it at this point
     (future-maybe-notify-stop me-f)
     (set-future*-state! me-f 'blocked)
     (on-transition-to-unfinished)
     (future-suspend)]
    [else
     (end-uninterruptible)]))

;; called in a Racket thread running a would-be future or as an unblock thread;
;; only does something if the thread matches the future's unblock thread
(define (future-unblock)
  (when (not-atomic-mode?)
    (define me-f (current-parallel-future-in-racket-thread))
    (when me-f
      (assert (continuation-prompt-available? future-start-prompt-tag))
      (lock-acquire (future*-lock me-f))
      (assert (eq? (future*-state me-f) #f))
      (with-continuation-mark
          break-enabled-key parallel-break-disabled-cell
          (future-suspend #:reschedule? #t
                          #:reschedule (lambda ()
                                         (current-future #f)
                                         ;; back to start, which will suspend and then
                                         ;; loop to potentially (if resumed) unblock again
                                         (unsafe-abort-current-continuation/no-wind future-start-prompt-tag (void))))))))

;; called with lock held on the current future, which implies
;; that `(current-atomic)` has been incremented, too
(define (future-suspend [touching-f #f]
                        #:reschedule? [reschedule? #f]
                        #:reschedule [reschedule #f])
  (define me-f (current-future))
  (unsafe-call-with-composable-continuation/no-wind
   (lambda (k)
     (cond
       [(eqv? (current-atomic) 1)
        (set-future*-thunk! me-f (if (and (future*-parallel me-f)
                                          (in-future-thread?))
                                     (lambda ()
                                       ;; check for break on apply in Racket thread,
                                       ;; since `no-wind` won't check automatically
                                       (call-in-continuation k check-for-break))
                                     k))]
       [else
        ;; extra atomicity is from `start-uninterrupted`s
        (internal-error "attempting to suspend a future in uninterruptible mode")])
     ;; no future-scheduler swap out from here on:
     (unless (in-racket-thread?)
       (define p (future*-parallel me-f))
       (when p         
         (set-scheduler-round-robin! (parallel-thread-pool-scheduler (parallel*-pool p)) 'pause)))         
     (define timestamp (and (not (future*-kind me-f))
                            (current-inexact-milliseconds)))
     (when timestamp
       ;; delay 'block/'sync to make sure it's worth computing `(continuation-current-primitive* k)`
       (set-future*-suspend-pthread-id! me-f (get-pthread-id))
       (set-future*-suspend-timestamp! me-f timestamp))
     (when reschedule?
       (schedule-future! me-f))
     ;; Release lock and go out of atomic mode:
     (lock-release (future*-lock me-f))
     (when touching-f
       (log-future 'touch (future*-id me-f) #:data (future*-id touching-f)
                   #:timestamp (or timestamp
                                   (current-inexact-milliseconds))))
     (when timestamp
       (log-future 'suspend (future*-id me-f) #:timestamp timestamp))
     (cond
       [reschedule
        (reschedule)]
       [(future*-kind me-f)
        (current-future #f)
        (set-future*-kind! me-f 'was)
        (unsafe-abort-current-continuation/no-wind future-start-prompt-tag (void))]
       [else
        (wakeup-racket-thread me-f)
        (unsafe-abort-current-continuation/no-wind future-scheduler-prompt-tag (void))]))
   future-start-prompt-tag))

(define (future-swapping-out? f)
  (eq? (scheduler-round-robin (parallel-thread-pool-scheduler (parallel*-pool (future*-parallel f)))) 'pause))

;; in future's own pthread and in uninterrupted mode
(define (wakeup-racket-thread me-f)
  (define p (future*-parallel me-f))
  (when p
    (unless (parallel*-stop? p)
      (define th (parallel*-thread p))
      (when th
        (assert (in-future-thread?))
        (set-engine-thread-cell-state! #f)
        (host:post-as-asynchronous-callback
         (lambda ()
           ;; in atomic mode and in arbitrary Racket thread selected by scheduler
           (cond
             [(thread-descheduled? th)
              ;; If the thread wasn't descheduled most recently by its future,
              ;; then the Racket thread could still have noticed the waiting
              ;; future thread early, ran it, and then get descheduled for some
              ;; other good reason
              (when (eq? 'future (thread-interrupt-callback th))
                (set-thread-interrupt-callback! th #f)
                (unless (or (is-thread-dead? th)
                            (thread-suspended? th))
                  (thread-reschedule! th)))]
             [else
              ;; Racket thread should be on its way back to `touch-blocked` or
              ;; already noticed the reader future; in the former case, it will
              ;; check on the future without needing to be rescheduled
              (void)]))))
      (wakeup-this-place))))

;; in atomic mode in Racket thread when an unblocking thread is killed or suspended;
;; the future can be in any state on entry; it ends up descheduled and not running
(define (future-external-stop f)
  (cond
    [(futures-enabled?)
     (lock-acquire (future*-lock f))
     (cond
       [(try-deschedule-future? f)
        (set-parallel*-stop?! (future*-parallel f) #t)
        (lock-release (future*-lock f))]
       [else
        (set-parallel*-stop?! (future*-parallel f) #t)
        (let loop ()
          (define done? (not (eq? (future*-state f) 'running)))
          (lock-release (future*-lock f))
          (unless done?
            (drain-async-callbacks)
            ;; relying on the fact that an asychornous callback post
            ;; will also wake up the place
            (sleep-this-place)
            (lock-acquire (future*-lock f))
            (loop)))])]
    [else
     ;; used by `call-in-future` on a would-be future
     (when (eq? (future*-state f) 'running)
       (set-future*-state! f 'stop))]))

;; lock on f is held
(define (future-maybe-notify-stop f)
  (define p (future*-parallel f))
  (when (and p
             (parallel*-stop? p)
             (eq? (future*-state f) 'running))
    (wakeup-this-place)))

;; in atomic mode in Racket thread when an unblocking thread is resumed
(define (future-external-resume f)
  (cond
    [(futures-enabled?)
     (lock-acquire (future*-lock f))
     (set-parallel*-stop?! (future*-parallel f) #f)
     (define th (parallel*-thread (future*-parallel f)))
     (when (or (not th)
               (eq? 'future (thread-interrupt-callback th)))
       (case (future*-state f)
         [(#f)
          (schedule-future! f)]
         [(blocked)
          (assert th)
          ;; the future may have queued a reschedule already, or maybe it
          ;; skipped the reschedule due to `paralle*-stop?` being set;
          ;; clear the interrupt callback so the thread can be rescheduled
          (set-thread-interrupt-callback! th #f)]))
     (lock-release (future*-lock f))]
    [else
     ;; used by `call-in-future` on a would-be future
     (when (eq? (future*-state f) 'stop)
       (set-future*-state! f 'running))]))

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
                   [round-robin #:mutable] ; #f, 'round, 'pause
                   [capacity #:mutable])
  #:authentic)

(struct worker (id
                [pthread #:mutable]
                current-future-box ; reports current future (for access external to pthread)
                [state #:mutable]
                [ping #:mutable]) ; box set to #t when the thread should check in with scheduler
  #:authentic)

(define current-scheduler
  (case-lambda
    [() (place-future-scheduler current-place)]
    [(s) (set-place-future-scheduler! current-place s)]))

(define (future-scheduler f)
  (define p (future*-parallel f))
  (if p
      (parallel-thread-pool-scheduler (parallel*-pool p))
      (current-scheduler)))

(define (make-worker id)
  (worker id
          #f         ; pthread
          (box #f)   ; current-future-box
          #f         ; state
          (box #f)))

;; called in a Racket thread
(define (maybe-start-scheduler)
  (atomically
   (unless (current-scheduler)
     (current-scheduler (start-scheduler pthread-count #f)))))

;; called in atomic mode in a Racket thread
(define (start-scheduler pthread-count round-robin?)
  (ensure-place-wakeup-handle)
  (define s (scheduler '()
                       #f  ; futures-head
                       #f  ; futures-tail
                       (host:make-mutex)
                       (host:make-condition)
                       (and round-robin? 'round)
                       pthread-count))
  (define workers
    (for/list ([id (in-range 1 (add1 pthread-count))])
      (define w (make-worker id))
      (start-worker w s)
      w))
  (set-scheduler-workers! s workers)
  s)

;; called in atomic mode
(define (kill-future-schedulers)
  (define s (current-scheduler))
  (when s
    (kill-future-scheduler s #:for-parallel? #f)
    (current-scheduler #f))
  (for ([s (in-hash-keys (place-schedulers current-place))])
    (kill-future-scheduler s))
  (set-place-schedulers! current-place (hasheq)))

;; called in atomic mode
(define (kill-future-scheduler s #:for-parallel? [for-parallel? #t])
  (scheduler-sync-for-shutdown s 'exit-request))

;; lock on f is held
(define (future-scheduled? f)
  (or (future*-prev f)
      (future*-next f)
       (and (not (future*-kind f))
            (eq? f (scheduler-futures-head (future-scheduler f))))))

;; called in any pthread
;; holding the lock for f or a unique reference to f
;; (see "future-lock.rkt" for more on lock discipline)
(define (schedule-future! f
                          #:front? [front? #f]
                          #:check-pool-open? [check-pool-open? #f])
  (start-uninterruptible)
  (assert (not (future-scheduled? f)))
  (assert (future*-thunk f))
  (assert (not (future*-state f)))
  (assert (let ([p (future*-parallel f)])
            (or (not p) (let ([th (parallel*-thread p)])
                          (or (not th) (not (thread-suspended? th)))))))
  (when (future*-parallel f)
    (increment-place-parallel-count! 1))
  (define s (future-scheduler f))
  (host:mutex-acquire (scheduler-mutex s))
  (when check-pool-open?
    (define pool (parallel*-pool (future*-parallel f)))
    (define capacity (sub1 (parallel-thread-pool-capacity pool)))
    (unless (capacity . >= . 0)
      (host:mutex-release (scheduler-mutex s))
      (raise-arguments-error 'thread/parallel "the parallel thread pool has been closed"))
    (set-parallel-thread-pool-capacity! pool capacity)
    (set-parallel-thread-pool-swimmers! pool (add1 (parallel-thread-pool-swimmers pool))))
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
  (host:mutex-release (scheduler-mutex s))
  (end-uninterruptible))

;; called with lock on f held, but not the queue lock
(define (try-deschedule-future? f #:decrement-count? [decrement-count? #t])
  (define s (future-scheduler f))
  (host:mutex-acquire (scheduler-mutex s))
  (define ok?
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
       (set-future*-next! f #f)
       #t]
      [(eq? f (scheduler-futures-head s))
       (set-scheduler-futures-head! s #f)
       (set-scheduler-futures-tail! s #f)
       #t]
      [else
       #f]))
  (host:mutex-release (scheduler-mutex s))
  (when (and ok? decrement-count? (future*-parallel f))
    (when (increment-place-parallel-count! -1)
      (wakeup-this-place)))
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
  (cond
    [(futures-enabled?)
     (lock-acquire (future*-lock f))
     (define p (future*-parallel f))
     (cond
       [(and p (parallel*-stop? p))
        (lock-release (future*-lock f))
        #f]
       [else
        (set-future*-state! f #f)
        (unless (future*-kind f)
          (schedule-future! f #:front? #t))
        (lock-release (future*-lock f))
        (on-transition-to-unfinished)
        (when (future*-kind f)
          (wakeup-this-place))
        #t])]
    [else
     ;; relevant to `call-in-future` on a would-be future
     (not (eq? (future*-state f) 'stop))]))

;; called in atomic mode in Racket thread or scheduling thread;
;; close a schduler when its thread pool will never have new work
(define (thread-pool-departure pool delta)
  (define s (parallel-thread-pool-scheduler pool))
  (host:mutex-acquire (scheduler-mutex s))
  (set-parallel-thread-pool-swimmers! pool (+ (parallel-thread-pool-swimmers pool) delta))
  (define capacity (parallel-thread-pool-capacity pool))
  (host:mutex-release (scheduler-mutex s))
  (when (zero? capacity)
    (kill-future-scheduler s)
    (set-place-schedulers! current-place (hash-remove (place-schedulers current-place) s))))

;; ----------------------------------------

(define-syntax-rule (keep-trying e)
  (let loop () (unless e (loop))))

(define (start-worker w s)
  (define th
    (fork-pthread
     (lambda ()
       (current-thread/in-racket #f)
       (current-future 'worker)
       (host:mutex-acquire (scheduler-mutex s))
       (let loop ()
         (cond
           [(eq? (worker-state w) 'exit-request)
            (set-worker-state! w 'exited)
            (host:mutex-release (scheduler-mutex s))
            (wakeup-this-place)]
           [(scheduler-futures-head s)
            => (lambda (f)
                 (worker-check-in w)
                 ;; give up scheduler lock in the hope of claiming f
                 (host:mutex-release (scheduler-mutex s))
                 (lock-acquire (future*-lock f))
                 (cond
                   [(try-deschedule-future? f #:decrement-count? #f)
                    (set-scheduler-capacity! s (- (scheduler-capacity s) 1))
                    ;; run the future
                    (maybe-run-future-in-worker f w s)
                    ;; look for more work
                    (host:mutex-acquire (scheduler-mutex s))
                    (set-scheduler-capacity! s (+ (scheduler-capacity s) 1))
                    (loop)]
                   [else
                    ;; future was claimed by someone else before
                    ;; we could deschedule it
                    (lock-release (future*-lock f))
                    (host:mutex-acquire (scheduler-mutex s))
                    (loop)]))]
           [else
            (worker-check-in w)
            ;; wait for work
            (host:condition-wait (scheduler-cond s) (scheduler-mutex s))
            (loop)])))))
  (set-worker-pthread! w th))

;; called with lock on f
(define (maybe-run-future-in-worker f w s)
  ;; Don't start the future if the custodian is shut down,
  ;; because we may have transitioned from 'pending to
  ;; 'running without an intervening check
  (cond
    [(or (custodian-shut-down?/other-pthread* (future*-custodian f))
         (future-stop? f))
     (future-maybe-notify-stop f)
     (set-future*-state! f #f)
     (on-transition-to-unfinished)
     (when (future*-parallel f)
       (when (increment-place-parallel-count! -1)
         (wakeup-this-place)))
     (lock-release (future*-lock f))]
    [else
     (run-future-in-worker f w s)]))

(define (run-future-in-worker f w s)
  (current-future f)
  (set-box! (worker-current-future-box w) f)
  ;; If we didn't need to check custodians, could be just
  ;;   (call-with-continuation-prompt
  ;;     (lambda () (run-future f))
  ;;     future-scheduler-prompt-tag
  ;;     void)
  ;; But use an engine so we can periodically check that the future is
  ;; still supposed to run.
  ;; We take advantage of `current-atomic` to disable interruptions,
  ;; both directly here and in the implementation of
  ;; `unsafe-{start, end}-uninterruptible`, so this is an example where
  ;; "atomic" really means "uninterruptible".
  (define e (make-engine (lambda ()
                           ;; includes balancing `(end-uninterruptible)` at start of thunk
                           (run-future f))
                         future-scheduler-prompt-tag
                         void
                         (make-engine-thread-cell-state break-enabled-default-cell
                                                        #t)
                         #t))
  (start-uninterruptible)
  (call-with-engine-completion
   (lambda (done)
     (let loop ([e e])
       (e TICKS
          (lambda ()
            (when (not-atomic-mode?)
              ;; Check in for termination request
              (define-values (exit? shut-down?)
                (cond
                  [(worker-pinged? w)
                   (host:mutex-acquire (scheduler-mutex s))
                   (define exit? (eq? (worker-state w) 'exit-request))
                   (define shut-down? (custodian-shut-down?/other-pthread* (future*-custodian f)))
                   (worker-check-in w)
                   (host:mutex-release (scheduler-mutex s))
                   (values exit? shut-down?)]
                  [else (values #f #f)]))
              (when (or exit?
                        shut-down?
                        (future-stop? f))
                (lock-acquire (future*-lock f))
                (future-maybe-notify-stop f)
                (set-future*-state! f #f)
                (on-transition-to-unfinished)
                (future-suspend))
              (when (eq? (scheduler-round-robin s) 'round)
                (check-for-break)
                (host:mutex-acquire (scheduler-mutex s))
                (define others? (and (scheduler-futures-head s)
                                     (zero? (scheduler-capacity s))))
                (host:mutex-release (scheduler-mutex s))
                (when others?
                  (lock-acquire (future*-lock f))
                  (future-maybe-notify-stop f)
                  (define stop? (future-stop? f))
                  (set-future*-state! f #f)
                  (future-suspend
                   #:reschedule? (not stop?)
                   #:reschedule (lambda ()
                                  (set-engine-thread-cell-state! #f)
                                  (unsafe-abort-current-continuation/no-wind future-scheduler-prompt-tag (void))))
                  (void)))))
          (lambda (e results leftover-ticks)
            (cond
              [e (loop e)]
              [else
               ;; Done --- completed or suspended (e.g., blocked)
               (when (future*-parallel f)
                 (when (increment-place-parallel-count! -1)
                   (wakeup-this-place)))
               (done (void))]))))))
  (log-future 'end-work (future*-id f))
  (current-future 'worker)
  (set-box! (worker-current-future-box w) #f)
  (when (scheduler-round-robin s)
    (set-scheduler-round-robin! s 'round)))

;; in atomic mode
(define (scheduler-sync-for-shutdown s request)
  ;; Make sure any futures that are running in a future pthread
  ;; have had a chance to notice a custodian shutdown or a
  ;; future-scheduler shutdown.
  (host:mutex-acquire (scheduler-mutex s))
  (for ([w (in-list (scheduler-workers s))])
    (unless (eq? (worker-state w) 'exited)
      (set-worker-state! w request)
      (let retry ()
        (unless (or (box-cas! (worker-ping w) #f #t)
                    (box-cas! (worker-ping w) #t #t))
          (retry)))))
  ;; Wake up idle threads so they check in:
  (host:condition-broadcast (scheduler-cond s))
  (host:mutex-release (scheduler-mutex s))
  ;; When a worker sets `state`, they must broadcast
  ;; a wakeup for the following loop's benefit
  (let loop ()
    (host:mutex-acquire (scheduler-mutex s))
    (define done? (for/or ([w (in-list (scheduler-workers s))])
                    (memq (worker-state w) (if (eq? request 'cust-request)
                                               '(exited cust)
                                               '(exited)))))
    (host:mutex-release (scheduler-mutex s))
    (unless done?
      (drain-async-callbacks)
      ;; relying on the fact that an asychornous callback post
      ;; will also wake up the place
      (sleep-this-place)
      (loop))))

;; in atomic mode
(define (futures-sync-for-shutdown)
  (when (current-scheduler)
    (scheduler-sync-for-shutdown (current-scheduler) 'cust-request)))

;; lock-free synchronization to check whether the box content is #f
(define (worker-pinged? w)
  (cond
    [(box-cas! (worker-ping w) #t #t) #t]
    [(box-cas! (worker-ping w) #f #f) #f]
    [else (worker-pinged? w)]))

;; scheduler lock is held
(define (worker-check-in w)
  (when (eq? (worker-state w) 'cust-request)
    (set-worker-state! w 'cust)
    (wakeup-this-place)))

;; in atomic mode
;; While we're trying to finish up futures, some of them
;; may be blocked waiting for async callbacks. No new ones
;; will get posted since we've set the ping flag, so we
;; only have to drain once.
(define (drain-async-callbacks)
  (define callbacks (host:poll-async-callbacks))
  (for ([callback (in-list callbacks)])
    (callback)))

;; ----------------------------------------

;; called in a GCing pthread with all other pthreads stopped
(define (scheduler-add-thread-custodian-mapping! s ht)
  (when s
    (for ([w (in-list (scheduler-workers s))])
      (define f (unbox (worker-current-future-box w)))
      (when f
        (define c (or (future*-custodian f)
                      (let ([th (parallel*-thread (future*-parallel f))])
                        (and th
                             (thread-representative-custodian th)))))
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
  (when (and me-f (in-future-thread?))
    (wakeup-this-place)))

(define wakeup-this-place (lambda () (void)))
(define sleep-this-place (lambda () (void)))
(define ensure-place-wakeup-handle (lambda () (void)))

(define (set-place-future-procs! wakeup sleep* ensure)
  (set! wakeup-this-place wakeup)
  (set! sleep-this-place sleep*)
  (set! ensure-place-wakeup-handle ensure))

;; tell "atomic.rkt" layer how to block:
(void (set-future-block! future-block future-unblock))

;; tell "custodian.rkt" how to sync and map pthreads to custodians:
(void (set-custodian-future-callbacks! futures-sync-for-shutdown
                                       scheduler-add-thread-custodian-mapping!))

;; tell "thread.rkt" layer how to maybe extract a thread from `(current-future)`:
(void (set-future->thread! (lambda (f)
                             (define p (future*-parallel f))
                             (and p (parallel*-thread p)))
                           future-swapping-out?))

(void (set-future-can-take-lock?! future*-parallel))
