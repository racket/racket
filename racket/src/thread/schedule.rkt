#lang racket/base
(require "config.rkt"
         "place-local.rkt"
         "place-object.rkt"
         "atomic.rkt"
         "host.rkt"
         "internal-error.rkt"
         "sandman.rkt"
         "parameter.rkt"
         "thread-group.rkt"
         "schedule-info.rkt"
         (submod "thread.rkt" scheduling)
         (submod "sync.rkt" scheduling)
         "system-idle-evt.rkt"
         "exit.rkt"
         "future.rkt"
         "custodian.rkt"
         (submod "custodian.rkt" scheduling)
         "pre-poll.rkt"
         "future-logging.rkt")

;; Many scheduler details are implemented in "thread.rkt", but this
;; module handles the thread selection, thread swapping, and
;; process sleeping.

(provide call-in-main-thread
         call-in-another-main-thread
         set-atomic-timeout-callback!
         set-check-place-activity!
         thread-swap-count)

;; Initializes the thread system:
(define (call-in-main-thread thunk)
  (call-in-new-main-thread
   (lambda ()
     (set-place-host-roots! initial-place (host:current-place-roots))
     (thunk))))

;; Initializes the thread system in a new place:
(define (call-in-another-main-thread c thunk)
  (make-another-initial-thread-group)
  (set-root-custodian! c)
  (init-system-idle-evt!)
  (init-future-place!)
  (init-schedule-counters!)
  (init-sync-place!)
  (call-in-new-main-thread thunk))

;; Finish initializing the thread system within a place:
(define (call-in-new-main-thread thunk)
  (make-initial-thread thunk)
  (call-with-engine-completion
   (lambda (done)
     (poll-and-select-thread! 0))))

;; ----------------------------------------

(define-place-local recent-process-milliseconds 0)
(define-place-local skipped-time-accums 0)
(define-place-local thread-swap-count 0)
(define (init-schedule-counters!)
  (set! recent-process-milliseconds 0)
  (set! skipped-time-accums 0)
  (set! thread-swap-count 0))
 
(define (poll-and-select-thread! leftover-ticks [pending-callbacks null])
  (define callbacks (if (null? pending-callbacks)
                        (host:poll-async-callbacks)
                        pending-callbacks))
  ;; Perform any expensive polls (such as ones that consult the OS)
  ;; only after ticks have been used up:
  (define poll-now? (leftover-ticks . <= . 0))
  (host:poll-will-executors)
  (poll-custodian-will-executor)
  (when poll-now?
    (check-external-events))
  (call-pre-poll-external-callbacks)
  (check-place-activity callbacks)
  (when (check-queued-custodian-shutdown)
    (when (thread-dead? root-thread)
      (force-exit 0)))
  (flush-future-log)
  (cond
    [(all-threads-poll-done?)
     ;; May need to sleep
     (cond
       [(not (null? callbacks))
        ;; Need to run atomic callbacks in some thread, so make one
        (do-make-thread 'callbacks
                        (lambda () (void))
                        #:custodian #f
                        #:at-root? #t)
        (poll-and-select-thread! TICKS callbacks)]
       [(and (not poll-now?)
             (check-external-events))
        ;; Retry and reset counter for checking external events
        (poll-and-select-thread! TICKS callbacks)]
       [(try-post-idle)
        ;; Enabled a thread that was waiting for idle
        (select-thread! leftover-ticks callbacks)]
       [else
        (process-sleep)
        ;; Retry, checking right away for external events
        (poll-and-select-thread! 0 callbacks)])]
    [else
     ;; Looks like some thread can work now
     (select-thread! (if poll-now? TICKS leftover-ticks) callbacks)]))

(define (select-thread! leftover-ticks callbacks)
  (let loop ([g root-thread-group] [callbacks callbacks] [none-k maybe-done])
    (define child (thread-group-next! g))
    (cond
      [(not child) (none-k callbacks)]
      [(thread? child)
       (swap-in-thread child leftover-ticks callbacks)]
      [else
       (loop child callbacks (lambda (callbacks) (loop g none-k callbacks)))])))

(define (swap-in-thread t leftover-ticks callbacks)
  (current-thread/in-atomic t)
  (define e (thread-engine t))
  ;; Remove `e` from the thread in `check-breaks-prefix`, in case
  ;; a GC happens between here and there, because `e` needs to
  ;; be attached to the thread for accounting purposes at a GC.
  (set-thread-sched-info! t #f)
  (current-future (thread-future t))
  (set-place-current-thread! current-place t)
  (set! thread-swap-count (add1 thread-swap-count))
  (run-callbacks-in-engine e callbacks t leftover-ticks))

(define (current-thread-now-running!)
  (set-thread-engine! (current-thread/in-atomic) 'running))

(define (swap-in-engine e t leftover-ticks)
  (assert-no-end-atomic-callbacks)
  (let loop ([e e] [prefix check-break-prefix])
    (end-implicit-atomic-mode)
    (e
     TICKS
     prefix
     (lambda (e results remaining-ticks)
       (start-implicit-atomic-mode)
       (cond
         [(not e)
          ;; Thread completed
          (accum-cpu-time! t #t)
          (set-thread-future! t #f)
          (current-thread/in-atomic #f)
          (set-place-current-thread! current-place #f)
          (current-future #f)
          (unless (zero? (current-atomic))
            (internal-error "terminated in atomic mode!"))
          (flush-end-atomic-callbacks!)
          (thread-dead! t)
          (when (eq? root-thread t)
            (force-exit 0))
          (thread-did-work!)
          (poll-and-select-thread! (- leftover-ticks (- TICKS remaining-ticks)))]
         [else
          ;; Thread continues
          (cond
            [(zero? (current-atomic))
             (when (thread-dead? root-thread)
               (force-exit 0))
             (define new-leftover-ticks (- leftover-ticks (- TICKS remaining-ticks)))
             (accum-cpu-time! t (new-leftover-ticks . <= . 0))
             (set-thread-future! t (current-future))
             (current-future #f)
             (set-place-current-thread! current-place #f)
             (unless (eq? (thread-engine t) 'done)
               (set-thread-engine! t e))
             (current-thread/in-atomic #f)
             (poll-and-select-thread! new-leftover-ticks)]
            [else
             ;; Swap out when the atomic region ends and at a point
             ;; where host-system interrupts are not disabled (i.e.,
             ;; don't use `engine-block` instead of `engine-timeout`):
             (add-end-atomic-callback! engine-timeout)
             (loop e check-for-atomic-timeout)])])))))

(define (check-break-prefix)
  (current-thread-now-running!)
  (check-for-break)
  (check-for-atomic-timeout))

(define (check-for-atomic-timeout)
  (when atomic-timeout-callback
    (when (positive? (current-atomic))
      (atomic-timeout-callback #f))))

(define (maybe-done callbacks)
  (cond
    [(pair? callbacks)
     ;; We have callbacks to run and no thread willing
     ;; to run them. Make a new thread.
     (do-make-thread 'scheduler-make-thread
                     void
                     #:custodian #f)
     (poll-and-select-thread! 0 callbacks)]
    [(and (not (sandman-any-sleepers?))
          (not (any-idle-waiters?)))
     ;; all threads done or blocked
     (cond
       [(thread-running? root-thread)
        ;; we shouldn't exit, because the main thread is
        ;; blocked, but it's not going to become unblocked;
        ;; sleep forever or until a signal changes things
        (process-sleep)
        (poll-and-select-thread! 0)]
       [else
        (void)])]
    [else
     ;; try again, which should lead to `process-sleep`
     (poll-and-select-thread! 0)]))

;; Check for threads that have been suspended until a particular time,
;; etc., as registered with the sandman
(define (check-external-events)
  (define did? #f)
  (sandman-poll (lambda (t)
                  (when t
                    (thread-reschedule! t))
                  (set! did? #t)))
  (when did?
    (thread-did-work!))
  did?)

;; Run callbacks within the thread for `e`, and don't give up until
;; the callbacks are done
(define (run-callbacks-in-engine e callbacks t leftover-ticks)
  (cond
    [(null? callbacks) (swap-in-engine e t leftover-ticks)]
    [else
     (define done? #f)
     (let loop ([e e] [callbacks callbacks])
       (end-implicit-atomic-mode)
       (e
        TICKS
        (if (pair? callbacks)
            ;; run callbacks as a "prefix" callback
            (lambda ()
              (current-thread-now-running!)
              (run-callbacks callbacks)
              (set! done? #t)
              (engine-block))
            ;; still running callbacks, so no new prefix
            void)
        (lambda (e result remaining)
          (start-implicit-atomic-mode)
          (unless e
            (internal-error "thread ended while it should run callbacks atomically"))
          (if done?
              (swap-in-engine e t leftover-ticks)
              (loop e null)))))]))

;; Run foreign "async-apply" callbacks, now that we're in some thread
(define (run-callbacks callbacks)
  (start-atomic)
  (for ([callback (in-list callbacks)])
    (callback))
  (end-atomic))

;; ----------------------------------------

;; Have we tried all threads since most recently making
;; progress on some thread?
(define (all-threads-poll-done?)
  (= (hash-count poll-done-threads)
     num-threads-in-groups))

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
