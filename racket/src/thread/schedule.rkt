#lang racket/base
(require "place-local.rkt"
         "place-object.rkt"
         "atomic.rkt"
         "host.rkt"
         "internal-error.rkt"
         "sandman.rkt"
         "parameter.rkt"
         "thread-group.rkt"
         "schedule-info.rkt"
         (submod "thread.rkt" scheduling)
         "system-idle-evt.rkt"
         "exit.rkt"
         "future.rkt"
         "custodian.rkt"
         (submod "custodian.rkt" scheduling)
         "pre-poll.rkt")

;; Many scheduler details are implemented in "thread.rkt", but this
;; module handles the thread selection, thread swapping, and
;; process sleeping.

(provide call-in-main-thread
         call-in-another-main-thread
         set-atomic-timeout-callback!
         set-check-place-activity!
         thread-swap-count)

(define TICKS 100000)

;; Initializes the thread system:
(define (call-in-main-thread thunk)
  (make-initial-thread (lambda ()
                         (set-place-host-roots! initial-place (host:current-place-roots))
                         (thunk)))
  (select-thread!))

;; Initializes the thread system in a new place:
(define (call-in-another-main-thread c thunk)
  (make-another-initial-thread-group)
  (set-root-custodian! c)
  (init-system-idle-evt!)
  (init-future-place!)
  (call-in-main-thread thunk)
  (init-schedule-counters!))

;; ----------------------------------------

(define-place-local recent-process-milliseconds 0)
(define-place-local skipped-time-accums 0)
(define-place-local thread-swap-count 0)
(define (init-schedule-counters!)
  (set! recent-process-milliseconds 0)
  (set! skipped-time-accums 0)
  (set! thread-swap-count 0))
 
(define (select-thread! [pending-callbacks null])
  (let loop ([g root-thread-group] [pending-callbacks pending-callbacks] [none-k maybe-done])
    (define callbacks (if (null? pending-callbacks)
                          (host:poll-async-callbacks)
                          pending-callbacks))
    (host:poll-will-executors)
    (poll-custodian-will-executor)
    (check-external-events 'fast)
    (call-pre-poll-external-callbacks)
    (check-place-activity)
    (check-queued-custodian-shutdown)
    (when (and (null? callbacks)
               (all-threads-poll-done?)
               (waiting-on-external-or-idle?))
      (or (check-external-events 'slow)
          (try-post-idle)
          (process-sleep)))
    (define child (thread-group-next! g))
    (cond
      [(not child) (none-k callbacks)]
      [(thread? child)
       (swap-in-thread child callbacks)]
      [else
       (loop child callbacks (lambda (pending-callbacks) (loop g none-k pending-callbacks)))])))

(define (swap-in-thread t callbacks)
  (define e (thread-engine t))
  (set-thread-engine! t 'running)
  (set-thread-sched-info! t #f)
  (current-thread t)
  (set-place-current-thread! current-place t)
  (set! thread-swap-count (add1 thread-swap-count))
  (run-callbacks-in-engine
   e callbacks
   (lambda (e)
     (let loop ([e e])
       (end-implicit-atomic-mode)
       (e
        TICKS
        (lambda ()
          (check-for-break)
          (when atomic-timeout-callback
            (when (positive? (current-atomic))
              (atomic-timeout-callback #f))))
        (lambda args
          (start-implicit-atomic-mode)
          (accum-cpu-time! t #t)
          (current-thread #f)
          (set-place-current-thread! current-place #f)
          (unless (zero? (current-atomic))
            (internal-error "terminated in atomic mode!"))
          (thread-dead! t)
          (when (eq? root-thread t)
            (force-exit 0))
          (thread-did-work!)
          (select-thread!))
        (lambda (e timeout?)
          (start-implicit-atomic-mode)
          (cond
            [(zero? (current-atomic))
             (accum-cpu-time! t timeout?)
             (current-thread #f)
             (set-place-current-thread! current-place #f)
             (unless (eq? (thread-engine t) 'done)
               (set-thread-engine! t e))
             (select-thread!)]
            [else
             ;; Swap out when the atomic region ends and at a point
             ;; where host-system interrupts are not disabled (i.e.,
             ;; don't use `engine-block` instead of `engine-timeout`):
             (add-end-atomic-callback! engine-timeout)
             (loop e)])))))))

(define (maybe-done callbacks)
  (cond
    [(pair? callbacks)
     ;; We have callbacks to run and no thread willing
     ;; to run them. Make a new thread.
     (do-make-thread 'scheduler-make-thread
                     void
                     #:custodian #f)
     (select-thread! callbacks)]
    [(and (not (sandman-any-sleepers?))
          (not (sandman-any-waiters?))
          (not (any-idle-waiters?)))
    ;; all threads done or blocked
    (cond
      [(thread-running? root-thread)
       ;; we shouldn't exit, because the main thread is
       ;; blocked, but it's not going to become unblocked;
       ;; sleep forever or until a signal changes things
       (process-sleep)
       (select-thread!)]
      [else
       (void)])]
   [else
    ;; try again, which should lead to `process-sleep`
    (select-thread!)]))

;; Check for threads that have been suspended until a particular time,
;; etc., as registered with the sandman
(define (check-external-events mode)
  (define did? #f)
  (sandman-poll mode
                (lambda (t)
                  (thread-reschedule! t)
                  (set! did? #t)))
  (sandman-condition-poll mode
                          (lambda (t)
                            (thread-reschedule! t)
                            (set! did? #t)))
  (when did?
    (thread-did-work!))
  did?)

;; Run callbacks within the thread for `e`, and don't give up until
;; the callbacks are done
(define (run-callbacks-in-engine e callbacks k)
  (cond
    [(null? callbacks) (k e)]
    [else
     (define done? #f)
     (let loop ([e e])
       (end-implicit-atomic-mode)
       (e
        TICKS
        (lambda ()
          (run-callbacks callbacks)
          (set! done? #t)
          (engine-block))
        (lambda args
          (internal-error "thread ended while it should run callbacks atomically"))
        (lambda (e timeout?)
          (start-implicit-atomic-mode)
          (if done?
              (k e)
              (loop e)))))]))

;; Run foreign "async-apply" callbacks, now that we're in some thread
(define (run-callbacks callbacks)
  (start-atomic)
  (for ([callback (in-list callbacks)])
    (callback))
  (end-atomic))

;; ----------------------------------------

;; Have we tried all threads without since most recently making
;; progress on some thread?
(define (all-threads-poll-done?)
  (= (hash-count poll-done-threads)
     num-threads-in-groups))

(define (waiting-on-external-or-idle?)
  (or (positive? num-threads-in-groups)
      (sandman-any-sleepers?)
      (any-idle-waiters?)))

;; Stop using the CPU for a while
(define (process-sleep)
  (define ts (thread-group-all-threads root-thread-group null))
  (define sleeping-exts
    (sandman-sleepers-external-events))
  (define exts
    (for/fold ([exts sleeping-exts]) ([t (in-list ts)])
      (define sched-info (thread-sched-info t))
      (define t-exts (and sched-info
                          (schedule-info-exts sched-info)))
      (sandman-merge-exts exts t-exts)))
  (sandman-sleep exts)
  ;; Maybe some thread can proceed:
  (thread-did-work!))

(define (try-post-idle)
  (and (post-idle)
       (begin
         (thread-did-work!)
         #t)))

;; ----------------------------------------

;; Getting CPU time is expensive relative to a thread
;; switch, so limit precision in the case that the thread
;; did not use up its quantum. This loss of precision
;; should be ok, since `(current-process-milliseconds <thread>)`
;; is used rarely, and it makes the most sense for threads
;; that don't keep swapping themselves out.

(define (accum-cpu-time! t timeout?)
  (cond
    [(not timeout?)
     (define n skipped-time-accums)
     (set! skipped-time-accums (add1 n))
     (when (= n 100)
       (accum-cpu-time! t #t))]
    [else
     (define start recent-process-milliseconds)
     (define now (current-process-milliseconds))
     (set! recent-process-milliseconds now)
     (set! skipped-time-accums 0)
     (set-thread-cpu-time! t (+ (thread-cpu-time t)
                                (- now start)))]))

;; ----------------------------------------

(define-place-local atomic-timeout-callback #f)

(define (set-atomic-timeout-callback! cb)
  (begin0
    atomic-timeout-callback
    (set! atomic-timeout-callback cb)))


(void (set-force-atomic-timeout-callback!
       (lambda ()
         (and atomic-timeout-callback
              (begin
                (atomic-timeout-callback #t)
                #t)))))

;; ----------------------------------------

(define check-place-activity void)
(define (set-check-place-activity! proc)
  (set! check-place-activity proc))
