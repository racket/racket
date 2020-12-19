#lang racket/base
(require "check.rkt"
         "internal-error.rkt"
         "evt.rkt"
         "atomic.rkt"
         "semaphore.rkt"
         "channel.rkt"
         (submod "channel.rkt" for-sync)
         "thread.rkt"
         "parameter.rkt"
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

(module+ scheduling
  (provide init-sync-place!))

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
                interrupt ; #f or a thunk to run on kill/break initiation
                abandons ; list of thunks to run on kill/break completion
                retry ; #f or a thunk to run on retry: returns `(values _val _ready?)`
                prev  ; previous in linked list
                next) ; next in linked list
  #:transparent
        #:mutable)

(define (make-syncer evt wraps prev)
  (syncer evt wraps null #f #f null #f prev #f))

(define none-syncer (make-syncer #f null #f))

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

  (define s (make-syncing (random-rotate (evts->syncers who args))
                          #:disable-break
                          (and local-break-cell
                               (let ([t (current-thread)])
                                 (lambda ()
                                   (thread-ignore-break-cell! t local-break-cell))))))

  (when (or (and (real? timeout) (zero? timeout))
            (procedure? timeout))
    (atomically
     (call-pre-poll-external-callbacks)
     (void)))

  ;; General polling loop
  (define (go #:thunk-result? [thunk-result? #t])
    (dynamic-wind
     (lambda ()
       (atomically
        (thread-push-kill-callback!
         (lambda () (syncing-abandon! s)))
        (thread-push-suspend+resume-callbacks!
         (lambda () (syncing-interrupt! s))
         (lambda () (syncing-queue-retry! s)))
        (void)))
     (lambda ()
       (when enable-break? (check-for-break))
       (cond
         [(or (and (real? timeout) (zero? timeout))
              (procedure? timeout))
          (let poll-loop ()
            (sync-poll s
                       #:success-k (and thunk-result? (lambda (thunk) thunk))
                       #:fail-k (lambda (sched-info polled-all? no-wrappers?)
                                  (cond
                                    [(not polled-all?)
                                     (poll-loop)]
                                    [(procedure? timeout)
                                     (if thunk-result?
                                         timeout
                                         (timeout))]
                                    [else
                                     (if thunk-result?
                                         (lambda () #f)
                                         #f)]))
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
               (cond
                 [(syncing-selected s)
                  ;; Selected after all:
                  (end-atomic)
                  (loop #f #f)]
                 [else
                  (syncing-done! s none-syncer)
                  (end-atomic)
                  ;; Return result:
                  (if thunk-result?
                      (lambda () #f)
                      #f)])]
              [(and (all-asynchronous? s)
                    (not (syncing-selected s))
                    (not (syncing-need-retry? s)))
               (suspend-syncing-thread s timeout-at)
               (set-syncing-wakeup! s void)
               (loop #f #t)]
              [else
               (sync-poll s
                          #:success-k (and thunk-result? (lambda (thunk) thunk))
                          #:did-work? did-work?
                          #:fail-k (lambda (sched-info now-polled-all? no-wrappers?)
                                     (when timeout-at
                                       (schedule-info-add-timeout-at! sched-info timeout-at))
                                     (thread-yield sched-info)
                                     (loop #f (or polled-all? now-polled-all?))))]))]))
     (lambda ()
       (atomically
        (thread-pop-suspend+resume-callbacks!)
        (thread-pop-kill-callback!)
        ;; On escape, post nacks, etc.:
        (syncing-abandon! s)
        (void)))))

  ;; Result thunk (if needed) is called in tail position:
  (cond
    [enable-break?
     ;; Install a new break cell, and check for breaks at the end:
     (define thunk (with-continuation-mark
                    break-enabled-key
                    local-break-cell
                    (go)))
     ;; If we get here, the break wasn't triggered, and it must be currently ignored.
     ;; (If the break was triggered so that we don't get here, it's not ignored.)
     (thread-remove-ignored-break-cell! (current-thread/in-atomic) local-break-cell)
     ;; In case old break cell was meanwhile enabled:
     (check-for-break)
     ;; In tail position:
     (thunk)]
    [else
     ;; Try a fast poll (avoiding `dynamic-wind`, etc.)
     ;; before chaining to `go`:
     (sync-poll s
                #:fail-k (lambda (sched-info polled-all? no-wrappers?)
                           (cond
                             [polled-all?
                              (cond
                                [(and (real? timeout) (zero? timeout)) #f]
                                [(procedure? timeout) (timeout)]
                                [no-wrappers? (go #:thunk-result? #f)]
                                [else ((go))])]
                             [else ((go))]))
                #:just-poll? #t
                #:fast-only? #t)]))

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
          (define sr (make-syncer arg wraps last))
          (unless (and (null? extended-commits)
                       (null? guarded-abandons))
            (set-syncer-commits! sr extended-commits)
            (set-syncer-abandons! sr guarded-abandons))
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
                   #:success-k [success-k #f] ; non-#f => result thunk passed to `success-k`
                   #:just-poll? [just-poll? #f]
                   #:fast-only? [fast-only? #f]
                   #:done-after-poll? [done-after-poll? #t]
                   #:did-work? [did-work? #f]
                   #:schedule-info [sched-info (make-schedule-info #:did-work? did-work?)])
  (let loop ([sr (syncing-syncers s)]
             [retries 0] ; count retries on `sr`, and advance if it's too many
             [polled-all-so-far? #t]
             [no-wrappers? #t])
    (start-atomic)
    (when (syncing-need-retry? s)
      (syncing-retry! s))
    (cond
      [(syncing-selected s)
       => (lambda (sr)
            ;; Some concurrent synchronization happened;
            ;; note that `make-result` is responsible for
            ;; exiting atomic mode
            (make-result sr (list (syncer-evt sr)) success-k))]
      [(not sr)
       (when (and just-poll? done-after-poll? polled-all-so-far? (not fast-only?))
         (syncing-done! s none-syncer))
       (end-atomic)
       (none-k sched-info polled-all-so-far? no-wrappers?)]
      [(= retries MAX-SYNC-TRIES-ON-ONE-EVT)
       (schedule-info-did-work! sched-info)
       (end-atomic)
       (loop (syncer-next sr) 0 #f #f)]
      [(nested-sync-evt? (syncer-evt sr))
       ;; Have to go out of atomic mode to continue:
       (end-atomic)
       (define-values (same? new-evt) (poll-nested-sync (syncer-evt sr) just-poll? fast-only? sched-info))
       (cond
         [same?
          (loop (syncer-next sr) 0 polled-all-so-far? no-wrappers?)]
         [else
          ;; Since we left atomic mode, double-check that we're
          ;; still syncing before installing the replacement event:
          (atomically
           (unless (syncing-selected s)
             (set-syncer-evt! sr new-evt))
           (void))
          (cond
            [fast-only?
             ;; Conservative, because we don't know whether `same?` was #f
             ;; because the nested sync had non-fast elements
             (none-k sched-info #f #f)]
            [else
             (loop sr (add1 retries) polled-all-so-far? no-wrappers?)])])]
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
          ;; `make-result` is responsible for leaving atomic mode
          (make-result sr results success-k)]
         [(delayed-poll? new-evt)
          ;; Have to go out of atomic mode to continue:
          (end-atomic)
          (cond
            [fast-only? (none-k sched-info #f #f)]
            [else
             (let ([new-evt ((delayed-poll-resume new-evt))])
               ;; Since we left atomic mode, double-check that we're
               ;; still syncing before installing the replacement event:
               (atomically
                (unless (syncing-selected s)
                  (set-syncer-evt! sr new-evt))
                (void))
               (loop sr (add1 retries) polled-all-so-far? #f))])]
         [(choice-evt? new-evt)
          (when (or (syncer-interrupt sr)
                    (syncer-retry sr))
            (end-atomic)
            (internal-error "choice event discovered after interrupt/retry callback"))
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
             (end-atomic)
             (loop (syncer-next sr) 0 polled-all-so-far? no-wrappers?)]
            [else
             ;; Splice in new syncers, and start there
             (syncer-replace! sr new-syncers s)
             (end-atomic)
             (loop new-syncers (add1 retries) polled-all-so-far? no-wrappers?)])]
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
             ;; `make-result` is responsible for leaving atomic mode
             (make-result sr (list always-evt) success-k)]
            [else
             (end-atomic)
             (loop sr (add1 retries) polled-all-so-far? #f)])]
         [(control-state-evt? new-evt)
          (define wrap-proc (control-state-evt-wrap-proc new-evt))
          (define interrupt-proc (control-state-evt-interrupt-proc new-evt))
          (define abandon-proc (control-state-evt-abandon-proc new-evt))
          (define retry-proc (control-state-evt-retry-proc new-evt))
          (unless (eq? wrap-proc values)
            (set-syncer-wraps! sr (cons wrap-proc (syncer-wraps sr))))
          (unless (eq? interrupt-proc void)
            (cond
              [(eq? interrupt-proc 'reset) (set-syncer-interrupt! sr #f)]
              [else
               (when (syncer-interrupt sr) (internal-error "syncer already has an interrupt callback"))
               (set-syncer-interrupt! sr interrupt-proc)]))
          (unless (eq? abandon-proc void)
            (set-syncer-abandons! sr (cons abandon-proc (syncer-abandons sr))))
          (unless (eq? retry-proc void)
            (cond
              [(eq? retry-proc 'reset) (set-syncer-retry! sr #f)]
              [else
               (when (syncer-retry sr) (internal-error "syncer already has an retry callback"))
               (set-syncer-retry! sr retry-proc)]))
          (set-syncer-evt! sr (control-state-evt-evt new-evt))
          (end-atomic)
          (cond
            [(and fast-only?
                  (not (and (eq? interrupt-proc void)
                            (eq? abandon-proc void)
                            (eq? retry-proc void))))
             (none-k sched-info #f #f)]
            [else
             (loop sr (add1 retries) polled-all-so-far? no-wrappers?)])]
         [(poll-guard-evt? new-evt)
          ;; Must leave atomic mode:
          (end-atomic)
          (cond
            [fast-only? (none-k sched-info #f #f)]
            [else
             (define generated (call-with-continuation-barrier
                                (lambda ()
                                  ((poll-guard-evt-proc new-evt) just-poll?))))
             (set-syncer-evt! sr (if (evt? generated)
                                     generated
                                     (wrap-evt always-evt (lambda (a) generated))))
             (loop sr (add1 retries) polled-all-so-far? #f)])]
         [(and (never-evt? new-evt)
               (not (evt-impersonator? new-evt))
               (not (syncer-interrupt sr))
               (null? (syncer-commits sr))
               (null? (syncer-abandons sr)))
          ;; Drop this event, since it will never get selected
          (syncer-remove! sr s)
          (end-atomic)
          (loop (syncer-next sr) 0 polled-all-so-far? no-wrappers?)]
         [(and (eq? new-evt (syncer-evt sr))
               (not (poll-ctx-incomplete? ctx)))
          ;; No progress on this evt
          (end-atomic)
          (loop (syncer-next sr) 0 polled-all-so-far? no-wrappers?)]
         [else
          (set-syncer-evt! sr new-evt)
          (end-atomic)
          (loop sr (add1 retries) polled-all-so-far? no-wrappers?)])])))

;; Called in atomic mode, but leaves atomic mode
;; Applies wraps immediately, while breaks are
;; potentially still disabled (but not in atomic mode), and then
;; returns another thunk to call a handler (if any) in tail position
(define (make-result sr results success-k)
  (define wraps (syncer-wraps sr))
  (end-atomic)
  (let loop ([wraps wraps] [results results])
    (cond
      [(null? wraps)
       (if success-k
           (success-k (lambda () (apply values results)))
           (apply values results))]
      [(null? (cdr wraps))
       ;; Call last one in tail position:
       (let ([proc (car wraps)])
         (if success-k
             (success-k (lambda () (apply proc results)))
             (apply proc results)))]
      [else
       (loop (cdr wraps)
             (call-with-values (lambda () (apply (car wraps) results)) list))])))

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
          (let ([interrupt (syncer-interrupt sr)])
            (when interrupt
              (interrupt))))
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
        (let ([interrupt (syncer-interrupt sr)])
          (when interrupt
            (interrupt))))
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
        (let ([retry (syncer-retry sr)])
          (when retry
            (define-values (result ready?) (retry))
            (when ready?
              (set-syncer-wraps! sr (cons (lambda args result) (syncer-wraps sr)))
              (syncing-done! s sr)))))
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
               (never-evt? e)
               (and (nested-sync-evt? e)
                    (let ([s (nested-sync-evt-s e)])
                      (and (not (syncing-selected s))
                           (all-asynchronous? s)))))
           (not (evt-impersonator? e))
           (loop (syncer-next sr)))]))))

;; In atomic mode
;; Gets nested syncings due to `replace-evt`, where they must
;; all have only asynchronous events
(define (nested-syncings s orig-s)
  (let loop ([sr (syncing-syncers s)])
    (cond
      [(not sr) null]
      [else
       (define e (syncer-evt sr))
       (cond
         [(nested-sync-evt? e)
          (define s (nested-sync-evt-s e))
          (set-syncing-wakeup! s
                               ;; In atomic mode
                               (lambda ()
                                 ((syncing-wakeup orig-s))))
          (append (nested-syncings s orig-s)
                  (cons s
                        (loop (syncer-next sr))))]
         [else
          (loop (syncer-next sr))])])))

;; Install a callback to reschedule the current thread if an
;; asynchronous selection happens, and then deschedule the thread
(define (suspend-syncing-thread s timeout-at)
  ((atomically
    (let retry ()
      (define nss (nested-syncings s s)) ; sets `syncing-wakeup` propagation
      (cond
       [(or (syncing-selected s)
            (for/or ([ns (in-list nss)])
              (syncing-selected ns)))
        ;; don't suspend after all
        void]
       [else
        (define t (current-thread/in-atomic))
        (set-syncing-wakeup!
         s
         ;; In atomic mode
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
                            ;; In atomic mode:
                            (lambda ()
                              ;; Interrupt due to break/kill/suspend
                              (set-syncing-wakeup! s void)
                              (unless (syncing-selected s)
                                (syncing-interrupt! s))
                              ;; In non-atomic mode and tail position:
                              (lambda ()
                                ;; Continue from suspend or ignored break...
                                ((atomically
                                  (unless (syncing-selected s)
                                    (syncing-retry! s))
                                  (retry))))))])))))

;; ----------------------------------------

(struct replacing-evt (guard)
  #:property prop:evt (poller (lambda (self poll-ctx) ((replacing-evt-guard self))))
  #:reflection-name 'evt)

(struct nested-sync-evt (s next orig-evt)
  #:property prop:evt (poller (lambda (self poll-ctx) (values #f self)))
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
         values
         ;; The interrupt and retry callbacks get discarded
         ;; when a new event is returned (but the abandon
         ;; callback is preserved)
         (lambda () (syncing-interrupt! s))
         (lambda () (syncing-abandon! s))
         (lambda () (syncing-retry! s)))))))
  orig-evt)

(define (poll-nested-sync ns just-poll? fast-only? sched-info)
  (sync-poll (nested-sync-evt-s ns)
             #:fail-k (lambda (sched-info polled-all? no-wrappers?)
                        (values polled-all? ns))
             #:success-k (lambda (thunk)
                           ;; `thunk` produces the values of the evt
                           ;; that was provided to `replace-evt`:
                           (define next (nested-sync-evt-next ns))
                           (define orig-evt (nested-sync-evt-orig-evt ns))
                           (values #f
                                   ;; and this is the "replace" step:
                                   (control-state-evt
                                    (poll-guard-evt
                                     (lambda (poll?)
                                       (define r (call-with-values thunk next))
                                       (cond
                                         [(evt? r) r]
                                         [else (wrap-evt always-evt (lambda (v) orig-evt))])))
                                    values
                                    'reset
                                    void
                                    'reset)))
             #:just-poll? just-poll?
             #:done-after-poll? #f
             #:fast-only? fast-only?
             #:schedule-info sched-info))

;; ----------------------------------------

(define/who current-evt-pseudo-random-generator
  (make-parameter (make-pseudo-random-generator)
                  (lambda (v)
                    (check who pseudo-random-generator? v)
                    v)
                  'current-evt-pseudo-random-generator))

(define (init-sync-place!)
  (current-evt-pseudo-random-generator (make-pseudo-random-generator)))

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
