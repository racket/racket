#lang racket/base
(require racket/unsafe/ops
         "check.rkt"
         "../common/queue.rkt"
         "internal-error.rkt"
         "host.rkt"
         "atomic.rkt"
         "parameter.rkt"
         "waiter.rkt"
         "evt.rkt"
         "pre-poll.rkt"
         "error.rkt"
         "place-local.rkt")

(provide make-semaphore
         semaphore?
         semaphore-post
         semaphore-post-all
         semaphore-wait
         semaphore-try-wait?
         
         semaphore-peek-evt
         semaphore-peek-evt?
         
         semaphore-any-waiters?

         semaphore-post/atomic
         semaphore-wait/atomic
         semaphore-post-all/atomic

         unsafe-semaphore-post
         unsafe-semaphore-wait
         unsafe-semaphore-try-wait?
         unsafe-semaphore-try-peek?)

(module+ for-thread
  ;; for creating subtypes in "thread.rkt"
  (provide (struct-out custodian-accessible-semaphore)
           semaphore))

(module+ for-future
  (provide set-future-can-take-lock?!))

(struct semaphore queue ([count #:mutable]) ; -1 => non-empty queue
  #:authentic
  #:property host:prop:unsafe-authentic-override #t ; allow evt chaperone
  #:property
  prop:evt
  (poller (lambda (s poll-ctx)
            (semaphore-wait/poll s s poll-ctx))))
(define count-field-pos 2) ; used with `unsafe-struct*-cas!`

;; When a thread is blocked on a custodian-accessible semaphore,
;; the the semaphore needs to be explicitly retained to ensure
;; that the blocking thread doesn't get GCed, since the custodian
;; might trigger it but holds a weak reference.
(struct custodian-accessible-semaphore semaphore ()
  #:authentic)

(struct semaphore-peek-evt (sema)
  #:property
  prop:evt
  (poller (lambda (sp poll-ctx)
            (semaphore-wait/poll (semaphore-peek-evt-sema sp)
                                 sp
                                 poll-ctx
                                 #:peek? #t
                                 #:result sp))))

(struct semaphore-peek-select-waiter select-waiter ())

(define/who (make-semaphore [init 0])
  (check who exact-nonnegative-integer? init)
  (unless (fixnum? init)
    (raise
     (exn:fail (error-message->string
                who
                (string-append "starting value "
                               (number->string init)
                               " is too large"))
               (current-continuation-marks))))
  (semaphore #f #f init))

;; ----------------------------------------

(define-place-local accessible-semaphores (hasheq))

(define (ready-nonempty-queue s)
  (when (queue-empty? s)
    (set-semaphore-count! s -1) ; so CAS not tried for `semaphore-post`
    (when (custodian-accessible-semaphore? s)
      (set! accessible-semaphores (hash-set accessible-semaphores s #t)))))

(define (ready-empty-queue s)
  (when (queue-empty? s)
    (set-semaphore-count! s 0) ; allow CAS again
    (when (custodian-accessible-semaphore? s)
      (set! accessible-semaphores (hash-remove accessible-semaphores s)))))

;; ----------------------------------------

(define/who (semaphore-post s)
  (check who semaphore? s)
  (unsafe-semaphore-post s))

(define (current-future-can-take-lock?)
  (or (current-thread/in-racket)
      (let ([f (current-future)])
        (or (not f)
            (future-can-take-lock? f)))))

(define (cas-only-mode?)
  (and (in-atomic-mode?)
       (not (current-thread/in-racket))  ; => really uninterruptible mode, not atomic mode
       (current-future))) ; => not in a foreign callback during a scheduler sleep (e.g., a GUI callback)

(define (unsafe-semaphore-post s)
  (define c (semaphore-count s))
  (cond
    [(and (c . >= . 0)
          (current-future-can-take-lock?)
          (begin
            (memory-order-release)
            (unsafe-struct*-cas! s count-field-pos c (add1 c))))
     (void)]
    [(cas-only-mode?)
     ;; ensure that an uncontested semaphore works in uninterruptible mode,
     ;; which means accommodating a spurious CAS failure
     (cond
       [(not (current-future-can-take-lock?))
        (internal-error "posted to a semaphore from a future in uninterruptible mode")]
       [(unsafe-struct*-cas! s count-field-pos -1 -1)
        (internal-error "posted in uninterruptible mode to a semaphore that has been contested")]
       [else
        ;; try again
        (unsafe-semaphore-post s)])]
    [else
     (atomically
      (semaphore-post/atomic s)
      (void))]))

;; In atomic mode:
(define (semaphore-post/atomic s)
  (assert-atomic-mode)
  (let loop ()
    (define w (queue-remove! s))
    (cond
      [(not w)
       (set-semaphore-count! s (add1 (semaphore-count s)))]
      [else
       (waiter-resume! w s)
       (ready-empty-queue s)
       (when (semaphore-peek-select-waiter? w)
         ;; Don't consume a post for a peek waiter
         (loop))])))

;; In atomic mode
(define (semaphore-post-all/atomic s)
  (assert-atomic-mode)
  (set-semaphore-count! s +inf.0)
  (queue-remove-all!
   s
   (lambda (w) (waiter-resume! w s)))
  (when (custodian-accessible-semaphore? s)
    (set! accessible-semaphores (hash-remove accessible-semaphores s))))

(define (semaphore-post-all s)
  (atomically
   (semaphore-post-all/atomic s)
   (void)))

;; In atomic mode:
(define (semaphore-any-waiters? s)
  (assert-atomic-mode)
  (not (queue-empty? s)))

;; ----------------------------------------

(define/who (semaphore-try-wait? s)
  (check who semaphore? s)
  (unsafe-semaphore-try-wait? s #t))

(define/who (unsafe-semaphore-try-wait? s decrement?)
  (define c (semaphore-count s))
  (cond
    [(and (positive? c)
          (current-future-can-take-lock?)
          (unsafe-struct*-cas! s count-field-pos c (if decrement? (sub1 c) c)))
     (memory-order-acquire)
     #t]
    [(cas-only-mode?)
     ;; ensure that an uncontested semaphore works in uninterruptible mode,
     ;; which means accommodating a spurious CAS failure
     (cond
       [(not (current-future-can-take-lock?))
        (internal-error "waited on a semaphore from a future in uninterruptible mode")]
       [(unsafe-struct*-cas! s count-field-pos c c)
        #f]
       [else
        (unsafe-semaphore-try-wait? s decrement?)])]
    [else
     (atomically
      (call-pre-poll-external-callbacks)
      (define c (semaphore-count s))
      (cond
        [(positive? c)
         (when decrement?
           (set-semaphore-count! s (sub1 c)))
         #t]
        [else #f]))]))

(define/who (unsafe-semaphore-try-peek? evt)
  (unsafe-semaphore-try-wait? (semaphore-peek-evt-sema evt) #f))

(define/who (semaphore-wait s)
  (check who semaphore? s)
  (unsafe-semaphore-wait s))

(define (unsafe-semaphore-wait s)
  (define c (semaphore-count s))
  (cond
    [(and (positive? c)
          (current-future-can-take-lock?)
          (unsafe-struct*-cas! s count-field-pos c (sub1 c)))
     (memory-order-acquire)]
    [(cas-only-mode?)
     ;; ensure that an uncontested semaphore works in uninterruptible mode,
     ;; which means accommodating a spurious CAS failure
     (cond
       [(not (current-future-can-take-lock?))
        (internal-error "waited on a semaphore from a future in uninterruptible mode")]
       [(unsafe-struct*-cas! s count-field-pos 0 0)
        (internal-error "waited in uninterruptible mode on a not-ready semaphore")]
       [else
        ;; try again
        (unsafe-semaphore-wait s)])]
    [else
     ((atomically/no-barrier-exit
       (define c (semaphore-count s))
       (cond
         [(positive? c)
          (set-semaphore-count! s (sub1 c))
          future-barrier-exit]
         [else
          (ready-nonempty-queue s)
          (define w (current-thread/in-racket))
          (define n (queue-add! s w))
          (waiter-suspend!
           w
           ;; On break/kill/suspend:
           (lambda ()
             (queue-remove-node! s n)
             (ready-empty-queue s)
             ;; This callback is used if the thread receives a break
             ;; signal but doesn't escape (either because breaks are
             ;; disabled or the handler continues), or if the
             ;; interrupt was to suspend and the thread is resumed:
             (lambda () (unsafe-semaphore-wait s))))])))]))

;; In atomic mode
(define (semaphore-wait/poll s self poll-ctx
                             #:peek? [peek? #f]
                             #:result [result s])
  ;; Similar to `semaphore-wait, but as called by `sync`,
  ;; so use a select waiter instead of the current thread
  (assert-atomic-mode)
  (define c (semaphore-count s))
  (cond
   [(positive? c)
    (unless peek?
      (set-semaphore-count! s (sub1 c)))
    (values (list result) #f)]
   [(poll-ctx-poll? poll-ctx)
    (values #f self)]
   [else
    (ready-nonempty-queue s)
    (define w (if peek?
                  (semaphore-peek-select-waiter (poll-ctx-select-proc poll-ctx))
                  (select-waiter (poll-ctx-select-proc poll-ctx))))
    (define n (queue-add! s w))
    ;; Replace with `async-evt`, but the `sema-waiter` can select the
    ;; event through a callback. Pair the event with a nack callback
    ;; to get back out of line.
    (values #f
            (control-state-evt async-evt
                               (lambda (v) result)
                               (lambda ()
                                 (assert-atomic-mode)
                                 (queue-remove-node! s n)
                                 (ready-empty-queue s))
                               void
                               (lambda ()
                                 ;; Retry: decrement or requeue
                                 (assert-atomic-mode)
                                 (define c (semaphore-count s))
                                 (cond
                                   [(positive? c)
                                    (unless peek?
                                      (set-semaphore-count! s (sub1 c)))
                                    (values result #t)]
                                   [else
                                    (ready-nonempty-queue s)
                                    (set! n (queue-add! s w))
                                    (values #f #f)]))))]))

;; Called only when it should immediately succeed:
(define (semaphore-wait/atomic s)
  (define c (semaphore-count s))
  (cond
    [(positive? c)
     (set-semaphore-count! s (sub1 c))]
    [else
     (internal-error "semaphore-wait/atomic: cannot decrement semaphore")]))

(define future-can-take-lock? (lambda (f) #f))

(define (set-future-can-take-lock?! pred)
  (set! future-can-take-lock? pred))
