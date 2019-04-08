#lang racket/base
(require racket/unsafe/ops
         "check.rkt"
         "../common/queue.rkt"
         "internal-error.rkt"
         "atomic.rkt"
         "parameter.rkt"
         "waiter.rkt"
         "evt.rkt"
         "pre-poll.rkt")

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
         unsafe-semaphore-wait)

(struct semaphore queue ([count #:mutable]) ; -1 => non-empty queue
  #:property
  prop:evt
  (poller (lambda (s poll-ctx)
            (semaphore-wait/poll s poll-ctx))))
(define count-field-pos 2) ; used with `unsafe-struct*-cas!`

(struct semaphore-peek-evt (sema)
  #:property
  prop:evt
  (poller (lambda (sp poll-ctx)
            (semaphore-wait/poll (semaphore-peek-evt-sema sp)
                                 poll-ctx
                                 #:peek? #t
                                 #:result sp))))

(struct semaphore-peek-select-waiter select-waiter ())

(define/who (make-semaphore [init 0])
  (check who exact-nonnegative-integer? init)
  (unless (fixnum? init)
    (raise
     (exn:fail (string-append
                "make-semaphore: starting value "
                (number->string init)
                " is too large")
               (current-continuation-marks))))
  (semaphore #f #f init))

;; ----------------------------------------

(define/who (semaphore-post s)
  (check who semaphore? s)
  (unsafe-semaphore-post s))

(define (unsafe-semaphore-post s)
  (define c (if (impersonator? s)
                -1
                (semaphore-count s)))
  (cond
    [(and (c . >= . 0)
          (unsafe-struct*-cas! s count-field-pos c (add1 c)))
     (void)]
    [else
     (atomically (semaphore-post/atomic s))]))

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
       (when (queue-empty? s)
         (set-semaphore-count! s 0)) ; allow CAS again
       (when (semaphore-peek-select-waiter? w)
         ;; Don't consume a post for a peek waiter
         (loop))])))

;; In atomic mode
(define (semaphore-post-all/atomic s)
  (set-semaphore-count! s +inf.0)
  (queue-remove-all!
   s
   (lambda (w) (waiter-resume! w s))))

(define (semaphore-post-all s)
  (atomically
   (semaphore-post-all/atomic s)))

;; In atomic mode:
(define (semaphore-any-waiters? s)
  (assert-atomic-mode)
  (not (queue-empty? s)))

;; ----------------------------------------

(define/who (semaphore-try-wait? s)
  (check who semaphore? s)
  (atomically
   (call-pre-poll-external-callbacks)
   (define c (semaphore-count s))
   (cond
     [(positive? c)
      (set-semaphore-count! s (sub1 c))
      #t]
     [else #f])))

(define/who (semaphore-wait s)
  (check who semaphore? s)
  (unsafe-semaphore-wait s))

(define (unsafe-semaphore-wait s)
  (define c (if (impersonator? s)
                -1
                (semaphore-count s)))
  (cond
    [(and (positive? c)
          (unsafe-struct*-cas! s count-field-pos c (sub1 c)))
     (void)]
    [else
     ((atomically
       (define c (semaphore-count s))
       (cond
         [(positive? c)
          (set-semaphore-count! s (sub1 c))
          void]
         [else
          (define w (current-thread))
          (define n (queue-add! s w))
          (set-semaphore-count! s -1) ; so CAS not tried for `semaphore-post`
          (waiter-suspend!
           w
           ;; On break/kill/suspend:
           (lambda ()
             (queue-remove-node! s n)
             (when (queue-empty? s)
               (set-semaphore-count! s 0))) ; allow CAS again
           ;; This callback is used, in addition to the previous one, if
           ;; the thread receives a break signal but doesn't escape
           ;; (either because breaks are disabled or the handler
           ;; continues), if if the interrupt was to suspend and the thread
           ;; is resumed:
           (lambda () (semaphore-wait s)))])))]))

;; In atomic mode
(define (semaphore-wait/poll s poll-ctx
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
    (values #f never-evt)]
   [else
    (define w (if peek?
                  (semaphore-peek-select-waiter (poll-ctx-select-proc poll-ctx))
                  (select-waiter (poll-ctx-select-proc poll-ctx))))
    (define n (queue-add! s w))
    (set-semaphore-count! s -1) ; so CAS not tried for `semaphore-post`
    ;; Replace with `async-evt`, but the `sema-waiter` can select the
    ;; event through a callback. Pair the event with a nack callback
    ;; to get back out of line.
    (values #f
            (wrap-evt
             (control-state-evt async-evt
                                (lambda ()
                                  (assert-atomic-mode)
                                  (queue-remove-node! s n)
                                  (when (queue-empty? s)
                                    (set-semaphore-count! s 0))) ; allow CAS again
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
                                    (set! n (queue-add! s w))
                                    (set-semaphore-count! s -1) ; so CAS not tried for `semaphore-post`
                                    (values #f #f)])))
             (lambda (v) result)))]))

;; Called only when it should immediately succeed:
(define (semaphore-wait/atomic s)
  (define c (semaphore-count s))
  (cond
    [(positive? c)
     (set-semaphore-count! s (sub1 c))]
    [else
     (internal-error "semaphore-wait/atomic: cannot decrement semaphore")]))
