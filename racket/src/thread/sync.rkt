#lang racket/base
(require "check.rkt"
         "internal-error.rkt"
         "evt.rkt"
         "atomic.rkt"
         "semaphore.rkt"
         "channel.rkt"
         (submod "channel.rkt" for-sync)
         "thread.rkt"
         (only-in (submod "thread.rkt" scheduling)
                  thread-descheduled?)
         "schedule-info.rkt"
         "pre-poll.rkt")

(provide sync
         sync/timeout
         sync/enable-break
         sync/timeout/enable-break
         sync-atomic-poll-evt?
         current-evt-pseudo-random-generator
         replace-evt)

(struct syncing (selected ; #f or a syncer that has been selected
                 syncers  ; linked list of `syncer`s
                 wakeup   ; a callback for when something is selected
                 disable-break ; #f or a thunk that disables breaks
                 need-retry?) ; queued trigger to `syncing-retry!`
        #:mutable)

(struct syncer (evt   ; the evt to sync; can get updated in sync loop
                wraps ; list of wraps to apply if selected
                commits ; list of thunks to run atomically when selected
                interrupted? ; kill/break in progress?
                interrupts ; list of thunks to run on kill/break initiation
                abandons ; list of thunks to run on kill/break completion
                retries ; list of thunks to run on retry: returns `(values _val _ready?)`
                prev  ; previous in linked list
                next) ; next in linked list
  #:transparent
        #:mutable)

(define (make-syncer evt wraps)
  (syncer evt wraps null #f null null null #f #f))

(define none-syncer (make-syncer #f null))

(define (make-syncing syncers #:disable-break [disable-break #f])
  (syncing #f ; selected
           syncers
           void ; wakeup
           disable-break
           #f))
  
;; To support `port-commit-peeked`, the `sync/timeout` function should
;; work for polling in atomic mode for a set of constrained event
;; types:
(define (sync-atomic-poll-evt? evt)
  (or (channel-put-evt? evt)
      (channel? evt)
      (semaphore? evt)
      (semaphore-peek-evt? evt)
      (eq? always-evt evt)
      (eq? never-evt evt)))

(define (do-sync who timeout args
                  #:enable-break? [enable-break? #f])
  (check who
         (lambda (timeout) (or (not timeout)
                               (and (real? timeout) (timeout . >= . 0))
                               (and (procedure? timeout)
                                    (procedure-arity-includes? timeout 0))))
         #:contract "(or/c #f (and/c real? (not/c negative?)) (-> any))"
         timeout)

  (define local-break-cell (and enable-break?
                                (make-thread-cell #t)))

  (define syncers (evts->syncers who args))
  (define s (make-syncing syncers
                          #:disable-break
                          (and local-break-cell
                               (let ([t (current-thread)])
                                 (lambda ()
                                   (thread-ignore-break-cell! t local-break-cell))))))

  (define (go)
    (dynamic-wind
     (lambda ()
       (atomically
        (thread-push-kill-callback!
         (lambda () (syncing-abandon! s)))
        (thread-push-suspend+resume-callbacks!
         (lambda () (syncing-interrupt! s))
         (lambda () (syncing-queue-retry! s)))))
     (lambda ()
       (when enable-break? (check-for-break))
       (cond
         [(or (and (real? timeout) (zero? timeout))
              (procedure? timeout))
          (atomically (call-pre-poll-external-callbacks))
          (let poll-loop ()
            (sync-poll s #:fail-k (lambda (sched-info polled-all?)
                                    (cond
                                      [(not polled-all?)
                                       (poll-loop)]
                                      [(procedure? timeout)
                                       timeout]
                                      [else
                                       (lambda () #f)]))
                       #:just-poll? #t))]
         [else
          ;; Loop to poll; if all events end up with asynchronous-select
          ;; callbacks, then the loop can suspend the current thread
          (define timeout-at
            (and timeout
                 (+ (* timeout 1000) (current-inexact-milliseconds))))
          (let loop ([did-work? #t] [polled-all? #f])
            (cond
              [(and polled-all?
                    timeout
                    (timeout-at . <= . (current-inexact-milliseconds)))
               (start-atomic)
               (syncing-done! s none-syncer)
               (cond
                 [(syncing-selected s)
                  ;; Selected after all:
                  (end-atomic)
                  (loop #f #f)]
                 [else
                  (end-atomic)
                  ;; Return result in a thunk:
                  (lambda () #f)])]
              [(and (all-asynchronous? s)
                    (not (syncing-selected s))
                    (not (syncing-need-retry? s)))
               (suspend-syncing-thread s timeout-at)
               (set-syncing-wakeup! s void)
               (loop #f #t)]
              [else
               (sync-poll s
                          #:did-work? did-work?
                          #:fail-k (lambda (sched-info now-polled-all?)
                                     (when timeout-at
                                       (schedule-info-add-timeout-at! sched-info timeout-at))
                                     (thread-yield sched-info)
                                     (loop #f (or polled-all? now-polled-all?))))]))]))
     (lambda ()
       (atomically
        (thread-pop-suspend+resume-callbacks!)
        (thread-pop-kill-callback!)
        (when local-break-cell
          (thread-remove-ignored-break-cell! (current-thread) local-break-cell))
        ;; On escape, post nacks, etc.:
        (syncing-abandon! s)))))
  
  ;; Result thunk is called in tail position:
  ((cond
     [enable-break?
      ;; Install a new break cell, and check for breaks at the end:
      (begin0
        (with-continuation-mark
         break-enabled-key
         local-break-cell
         (go))
        ;; In case old break cell was meanwhile enabled:
        (check-for-break))]
     [else
      ;; Just `go`:
      (go)])))

(define sync
  (case-lambda
    [(evt)
     (cond
       [(evt-impersonator? evt)
        (do-sync 'sync #f (list evt))]
       [(semaphore? evt)
        (semaphore-wait evt)
        evt]
       [(channel? evt)
        (channel-get evt)]
       [(channel-put-evt? evt)
        (channel-put-do evt)
        evt]
       [else
        (do-sync 'sync #f (list evt))])]
    [args
     (let ([simpler-args (simplify-evts args)])
       (if (and (pair? simpler-args) (null? (cdr simpler-args)))
           (sync (car simpler-args))
           (do-sync 'sync #f simpler-args)))]))

(define sync/timeout
  (case-lambda
    [(timeout evt)
     (cond
       [(evt-impersonator? evt)
        (do-sync 'sync/timeout timeout (list evt))]
       [(and (eqv? timeout 0)
             (semaphore? evt))
        (if (semaphore-try-wait? evt)
            evt
            #f)]
       [(not timeout)
        (cond
          [(semaphore? evt)
           (semaphore-wait evt)
           evt]
          [(channel? evt)
           (channel-get evt)]
          [(channel-put-evt? evt)
           (channel-put-do evt)
           evt]
          [else
           (do-sync 'sync/timeout #f (list evt))])]
       [else
        (do-sync 'sync/timeout timeout (list evt))])]
    [(timeout . args)
     (let ([simpler-args (simplify-evts args)])
       (if (and (pair? simpler-args) (null? (cdr simpler-args)))
           (sync/timeout timeout (car simpler-args))
           (do-sync 'sync/timeout timeout simpler-args)))]))

;; Filter `never-evt` and flatten small `choice-evt` in an
;; effort to expose simple cases, like just a semaphore
(define (simplify-evts args)
  (cond
    [(null? args) args]
    [else
     (let ([arg (car args)])
       (cond
         [(eq? never-evt arg)
          (simplify-evts (cdr args))]
         [(and (choice-evt? arg)
               ((length (choice-evt-evts arg)) . < . 3))
          (simplify-evts (append (choice-evt-evts arg) (cdr args)))]
         [else
          (cons arg (simplify-evts (cdr args)))]))]))

(define (sync/enable-break . args)
  (do-sync 'sync/enable-break #f args #:enable-break? #t))

(define (sync/timeout/enable-break timeout . args)
  (do-sync 'sync/timeout/enable-break timeout args #:enable-break? #t))

;; Resolve mutual dependency:
(void (set-sync-on-channel! sync))

;; ----------------------------------------

(define (evts->syncers who evts [wraps null] [commits null] [abandons null])
  (define-values (extended-commits guarded-abandons)
    (cross-commits-and-abandons commits abandons))
  (let loop ([evts evts] [first #f] [last #f])
    (cond
      [(null? evts) first]
      [else
       (define arg (car evts))
       (when who
         (check who evt? arg))
       (cond
         [(choice-evt? arg)
          ;; Splice choice events eagerly to improve fairness
          ;; of selection
          (loop (append (choice-evt-evts arg) (cdr evts))
                first
                last)]
         [else
          (define sr (make-syncer arg wraps))
          (unless (and (null? extended-commits)
                       (null? guarded-abandons))
            (set-syncer-commits! sr extended-commits)
            (set-syncer-abandons! sr guarded-abandons))
          (set-syncer-prev! sr last)
          (when last
            (set-syncer-next! last sr))
          (loop (cdr evts)
                (or first sr)
                sr)])])))

(define (cross-commits-and-abandons commits abandons)
  (cond
    [(and (null? commits) (null? abandons))
     (values null null)]
    [else
     (define selected? #f)
     (values (list
              ;; in atomic mode
              (lambda ()
                (assert-atomic-mode)
                (set! selected? #t)
                (for ([commit (in-list commits)])
                  (commit))
                (set! commits null)))
             (list
              ;; in atomic mode
              (lambda ()
                (assert-atomic-mode)
                (unless selected?
                  (for ([abandon (in-list abandons)])
                    (abandon)))
                (set! abandons null))))]))

;; in atomic mode
;; remove a syncer from its chain in `s`
(define (syncer-remove! sr s)
  (assert-atomic-mode)
  (if (syncer-prev sr)
      (set-syncer-next! (syncer-prev sr) (syncer-next sr))
      (set-syncing-syncers! s (syncer-next sr)))
  (when (syncer-next sr)
    (set-syncer-prev! (syncer-next sr) (syncer-prev sr))))

;; in atomic mode
;; Replace one syncer with a new, non-empty chain of syncers in `s`
(define (syncer-replace! sr new-syncers s)
  (assert-atomic-mode)
  (let ([prev (syncer-prev sr)])
    (set-syncer-prev! new-syncers prev)
    (if prev
        (set-syncer-next! prev new-syncers)
        (set-syncing-syncers! s new-syncers)))
  (let loop ([new-syncers new-syncers])
    (cond
      [(syncer-next new-syncers)
       => (lambda (next) (loop next))]
      [else
       (let ([next (syncer-next sr)])
         (set-syncer-next! new-syncers next)
         (when next
           (set-syncer-prev! next new-syncers)))])))

;; ----------------------------------------

(define MAX-SYNC-TRIES-ON-ONE-EVT 10)

;; Run through the events of a `sync` one time; returns a thunk to
;; call in tail position --- possibly one that calls `none-k`.
(define (sync-poll s
                   #:fail-k none-k
                   #:success-k [success-k (lambda (thunk) thunk)]
                   #:just-poll? [just-poll? #f]
                   #:done-after-poll? [done-after-poll? #t]
                   #:did-work? [did-work? #f]
                   #:schedule-info [sched-info (make-schedule-info #:did-work? did-work?)])
  (random-rotate-syncing! s)
  (let loop ([sr (syncing-syncers s)]
             [retries 0] ; count retries on `sr`, and advance if it's too many
             [polled-all-so-far? #t])
    (when (syncing-need-retry? s)
      (syncing-retry! s))
    ((atomically
      (cond
       [(syncing-selected s)
        => (lambda (sr)
             ;; Some concurrent synchronization happened
             (make-result-thunk sr (list (syncer-evt sr)) success-k))]
       [(not sr)
        (when (and just-poll? done-after-poll? polled-all-so-far?)
          (syncing-done! s none-syncer))
        (lambda () (none-k sched-info polled-all-so-far?))]
       [(= retries MAX-SYNC-TRIES-ON-ONE-EVT)
        (schedule-info-did-work! sched-info)
        (lambda () (loop (syncer-next sr) 0 #f))]
       [else
        (define ctx (poll-ctx just-poll?
                              ;; Call back for asynchronous selection,
                              ;; such as by a semaphore when it's posted
                              ;; in a different thread; this callback
                              ;; must be invoked in atomic mode
                              (lambda ()
                                (assert-atomic-mode)
                                (syncing-done! s sr))
                              ;; Information to propagate to the thread
                              ;; scheduler
                              sched-info
                              ;; Set to #t if getting the same result
                              ;; back should not be treated as a
                              ;; completed poll:
                              #f))
        (define-values (results new-evt)
          (evt-poll (syncer-evt sr) ctx))
        (cond
         [results
          (syncing-done! s sr)
          (make-result-thunk sr results success-k)]
         [(delayed-poll? new-evt)
          ;; Have to go out of atomic mode to continue:
          (lambda ()
            (let ([new-evt ((delayed-poll-resume new-evt))])
              ;; Since we left atomic mode, double-check that we're
              ;; still syncing before installing the replacement event:
              (atomically
               (unless (syncing-selected s)
                 (set-syncer-evt! sr new-evt)))
              (loop sr (add1 retries) polled-all-so-far?)))]
         [(choice-evt? new-evt)
          (when (or (pair? (syncer-interrupts sr))
                    (pair? (syncer-retries sr)))
            (internal-error "choice event discovered after interrupt/retry callbacks"))
          (define new-syncers (random-rotate
                               (evts->syncers #f
                                              (choice-evt-evts new-evt)
                                              (syncer-wraps sr)
                                              (syncer-commits sr)
                                              (syncer-abandons sr))))
          (cond
            [(not new-syncers)
             ;; Empty choice, so drop it:
             (syncer-remove! sr s)
             (lambda () (loop (syncer-next sr) 0 polled-all-so-far?))]
            [else
             ;; Splice in new syncers, and start there
             (syncer-replace! sr new-syncers s)
             (lambda () (loop new-syncers (add1 retries) polled-all-so-far?))])]
         [(wrap-evt? new-evt)
          (set-syncer-wraps! sr (cons (wrap-evt-wrap new-evt)
                                      (let ([l (syncer-wraps sr)])
                                        (if (and (null? l)
                                                 (not (handle-evt? new-evt)))
                                            ;; Prevent wrapper from being in tail position:
                                            (list values)
                                            ;; Allow handler in tail position:
                                            l))))
          (define inner-new-evt (wrap-evt-evt new-evt))
          (set-syncer-evt! sr inner-new-evt)
          ;; In support of the `poller` protocol, if the new evt is
          ;; `always-evt`, then select it immediately
          (cond
            [(eq? inner-new-evt always-evt)
             (syncing-done! s sr)
             (make-result-thunk sr (list always-evt) success-k)]
            [else
             (lambda () (loop sr (add1 retries) polled-all-so-far?))])]
         [(control-state-evt? new-evt)
          (set-syncer-interrupts! sr (cons-non-void (control-state-evt-interrupt-proc new-evt) (syncer-interrupts sr)))
          (set-syncer-abandons! sr (cons-non-void (control-state-evt-abandon-proc new-evt) (syncer-abandons sr)))
          (set-syncer-retries! sr (cons-non-void (control-state-evt-retry-proc new-evt) (syncer-retries sr)))
          (set-syncer-evt! sr (control-state-evt-evt new-evt))
          (lambda () (loop sr (add1 retries) polled-all-so-far?))]
         [(poll-guard-evt? new-evt)
          (lambda ()
            ;; Out of atomic region:
            (define generated ((poll-guard-evt-proc new-evt) just-poll?))
            (set-syncer-evt! sr (if (evt? generated)
                                    generated
                                    (wrap-evt always-evt (lambda (a) generated))))
            (loop sr (add1 retries) polled-all-so-far?))]
         [(and (never-evt? new-evt)
               (not (evt-impersonator? new-evt))
               (null? (syncer-interrupts sr))
               (null? (syncer-commits sr))
               (null? (syncer-abandons sr)))
          ;; Drop this event, since it will never get selected
          (syncer-remove! sr s)
          (lambda () (loop (syncer-next sr) 0 polled-all-so-far?))]
         [(and (eq? new-evt (syncer-evt sr))
               (not (poll-ctx-incomplete? ctx)))
          ;; No progress on this evt
          (lambda () (loop (syncer-next sr) 0 polled-all-so-far?))]
         [else
          (set-syncer-evt! sr new-evt)
          (lambda () (loop sr (add1 retries) polled-all-so-far?))])])))))

;; Create a thunk that applies wraps immediately, while breaks are
;; potentially still disabled (but not in atomic mode), and then
;; returns another thunk to call a handler (if any) in tail position
(define (make-result-thunk sr results success-k)
  (define wraps (syncer-wraps sr))
  (lambda ()
    (let loop ([wraps wraps] [results results])
      (cond
        [(null? wraps)
         (success-k
          (lambda ()
            (apply values results)))]
        [(null? (cdr wraps))
         ;; Call last one in tail position:
         (let ([proc (car wraps)])
           (success-k
            (lambda ()
              (apply proc results))))]
        [else
         (loop (cdr wraps)
               (call-with-values (lambda () (apply (car wraps) results)) list))]))))

(define (cons-non-void a d)
  (if (eq? a void)
      d
      (cons a d)))

;; ----------------------------------------

;; Called in atomic mode
;;  Finalizes a decision for the sychronization, calling
;;  interrupt+abandon (or just abandon, if already interrupted)
;;  on non-selected events to indicate that they will never be
;;  selected for this synchronization
(define (syncing-done! s selected-sr)
  (assert-atomic-mode)
  (set-syncing-selected! s selected-sr)
  (for ([callback (in-list (syncer-commits selected-sr))])
    (callback))
  (let loop ([sr (syncing-syncers s)])
    (when sr
      (unless (eq? sr selected-sr)
        (unless (syncer-interrupted? sr)
          (for ([interrupt (in-list (syncer-interrupts sr))])
            (interrupt)))
        (for ([abandon (in-list (syncer-abandons sr))])
          (abandon)))
      (loop (syncer-next sr))))
  (when (syncing-disable-break s)
    ((syncing-disable-break s)))
  ((syncing-wakeup s)))

;; Called in atomic mode
(define (syncing-abandon! s)
  (assert-atomic-mode)
  (unless (syncing-selected s)
    (syncing-done! s none-syncer)))

;; Called in atomic mode
;;  For each syncer that needs a notification (e.g., to get out of
;;  a queue of waiters), call its `interrupt` callback
(define (syncing-interrupt! s)
  (assert-atomic-mode)
  (let loop ([sr (syncing-syncers s)])
    (when sr
      (unless (syncer-interrupted? sr)
        (set-syncer-interrupted?! sr #t)
        (for ([interrupt (in-list (syncer-interrupts sr))])
          (interrupt)))
      (loop (syncer-next sr)))))

;; Called in atomic mode
;;  For each syncer that needs a notification (e.g., to get back into
;;  a queue of waiters), call its `retry` callback; a retry might
;;  succeed immediately, moving the synchronization into "selected"
;;  state
(define (syncing-retry! s)
  (assert-atomic-mode)
  (set-syncing-need-retry?! s #f)
  (let loop ([sr (syncing-syncers s)])
    (when (and sr
               (not (syncing-selected s)))
      (when (syncer-interrupted? sr)
        (set-syncer-interrupted?! sr #f)
        ;; Although we keep a list of retries, we expect only
        ;; one to be relevant
        (for ([retry (in-list (syncer-retries sr))])
          (define-values (result ready?) (retry))
          (when ready?
            (set-syncer-wraps! sr (cons (lambda args result) (syncer-wraps sr)))
            (syncing-done! s sr))))
      (loop (syncer-next sr)))))

;; Queue a retry when a check for breaks should happen before a retry
;; that might immediately succeed
(define (syncing-queue-retry! s)
  (set-syncing-need-retry?! s #t))

;; ----------------------------------------

;; If everything we're waiting on is like a semaphore or channel,
;; where an asynchronous selection event is installed, then we can
;; completely suspend this thread
(define (all-asynchronous? s)
  (atomically
   (let loop ([sr (syncing-syncers s)])
    (cond
     [(not sr) #t]
     [else
      (define e (syncer-evt sr))
      (and (or (async-evt? e)
               (never-evt? e))
           (not (evt-impersonator? e))
           (loop (syncer-next sr)))]))))

;; Install a callback to reschedule the current thread if an
;; asynchronous selection happens, and then deschedule the thread
(define (suspend-syncing-thread s timeout-at)
  ((atomically
    (let retry ()
      (cond
       [(syncing-selected s)
        ;; don't suspend after all
        void]
       [else
        (define t (current-thread))
        (set-syncing-wakeup!
         s
         (lambda ()
           (set-syncing-wakeup! s void)
           ;; In case this callback is late, where the thread was
           ;; already rescheduled for some reason:
           (when (thread-descheduled? t)
             (thread-reschedule! t))))
        ;; Suspend and resume callbacks will also
        ;; interrupt and queue a retry, but it's ok
        ;; to have both at this point
        (thread-deschedule! t
                            timeout-at
                            (lambda ()
                              ;; Interrupt due to break/kill/suspend
                              (set-syncing-wakeup! s void)
                              (unless (syncing-selected s)
                                (syncing-interrupt! s)))
                            (lambda ()
                              ;; Continue from suspend or ignored break...
                              ;; In non-atomic mode and tail position:
                              ((atomically
                                (unless (syncing-selected s)
                                  (syncing-retry! s))
                                (retry)))))])))))

;; ----------------------------------------

(struct replacing-evt (guard)
  #:property prop:evt (poller (lambda (self poll-ctx) ((replacing-evt-guard self))))
  #:reflection-name 'evt)

(struct nested-sync-evt (s next orig-evt)
  #:property prop:evt (poller (lambda (self poll-ctx) (poll-nested-sync self poll-ctx)))
  #:reflection-name 'evt)

(define/who (replace-evt evt next)
  (check who evt? evt)
  (check who procedure? next)
  (define orig-evt
    (replacing-evt
     ;; called for each `sync`:
     (lambda ()
       (define s (make-syncing (evts->syncers who (list evt))))
       (values
        #f
        ;; represents the instantited attempt to sync on `evt`:
        (control-state-evt
         (nested-sync-evt s next orig-evt)
         (lambda () (syncing-interrupt! s))
         (lambda () (syncing-abandon! s))
         (lambda () (syncing-retry! s)))))))
  orig-evt)

(define (poll-nested-sync ns poll-ctx)
  (sync-poll (nested-sync-evt-s ns)
             #:fail-k (lambda (sched-info polled-all?)
                        (unless polled-all?
                          (set-poll-ctx-incomplete?! poll-ctx #f))
                        (values #f ns))
             #:success-k (lambda (thunk)
                           ;; `thunk` produces the values of the evt
                           ;; that was provided to `replace-evt`:
                           (define next (nested-sync-evt-next ns))
                           (define orig-evt (nested-sync-evt-orig-evt ns))
                           (values #f
                                   ;; and this is the "replace" step:
                                   (poll-guard-evt
                                    (lambda (poll?)
                                      (define r (call-with-values thunk next))
                                      (cond
                                        [(evt? r) r]
                                        [else (wrap-evt always-evt (lambda (v) orig-evt))])))))
             #:just-poll? (poll-ctx-poll? poll-ctx)
             #:done-after-poll? #f
             #:schedule-info (poll-ctx-sched-info poll-ctx)))

;; ----------------------------------------

(define/who current-evt-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)
                  (lambda (v)
                    (check who pseudo-random-generator? v)
                    v)))

;; rotates the order of syncers in `s` to implement fair selection:
(define (random-rotate-syncing! s)
  (set-syncing-syncers! s (random-rotate (syncing-syncers s))))

(define (random-rotate first-sr)
  (define n (let loop ([sr first-sr] [n 0])
              (cond
                [(not sr) n]
                [else (loop (syncer-next sr) (add1 n))])))
  (cond
    [(n . <= . 1) first-sr]
    [else
     (define m (random n (current-evt-pseudo-random-generator)))
     (cond
       [(zero? m) first-sr]
       [else
        (let loop ([sr first-sr] [m (sub1 m)])
          (cond
            [(zero? m)
             (define new-first-sr (syncer-next sr))
             (set-syncer-next! sr #f)
             (set-syncer-prev! new-first-sr #f)
             (let loop ([next-sr new-first-sr])
               (define next-next-sr (syncer-next next-sr))
               (cond
                 [(not next-next-sr)
                  (set-syncer-next! next-sr first-sr)
                  (set-syncer-prev! first-sr next-sr)
                  new-first-sr]
                 [else (loop next-next-sr)]))]
            [else
             (loop (syncer-next sr) (sub1 m))]))])]))
