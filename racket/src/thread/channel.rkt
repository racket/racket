#lang racket/base
(require "check.rkt"
         "atomic.rkt"
         "parameter.rkt"
         "evt.rkt"
         "waiter.rkt"
         "semaphore.rkt"
         "../common/queue.rkt")

(provide make-channel
         channel?
         channel-put
         channel-get
         
         channel-put-evt
         channel-put-evt?
         channel-put-do

         channel-get-poll-or-semaphore
         channel-put-poll-or-semaphore)

(module+ for-sync
  (provide set-sync-on-channel!))

(module+ for-impersonator
  (provide impersonator-prop:channel-put
           channel-put-impersonator?
           channel-put-impersonator-ref))

;; ----------------------------------------

(struct channel (get-queue
                 put-queue
                 [get-queued-sema #:mutable]
                 [put-queued-sema #:mutable])
  #:property
  prop:evt
  (poller (lambda (ch poll-ctx)
            (channel-get/poll ch poll-ctx))))

(struct channel-put-evt* (ch v)
  #:property
  prop:evt
  (poller (lambda (cp poll-ctx)
            (channel-put/poll (channel-put-evt*-ch cp)
                              (channel-put-evt*-v cp)
                              cp
                              poll-ctx)))
  #:reflection-name 'channel-put-evt)

;; A channel must not match get and put from the same thread, which is
;; a danger when `sync` queues up multiple events at a time:
(struct channel-select-waiter select-waiter (thread))

(define (make-channel)
  (channel (make-queue) (make-queue) #f #f))

;; ----------------------------------------

(define/who (channel-get ch)
  (check who channel? ch)
  (cond
    [(evt-impersonator? ch)
     ;; Use the more general path to get impersonator handling:
     (sync-on-channel ch)]
    [else
     (define b (box #f))
     (let receive () ; loop if a retry is needed
       ((atomically/no-barrier-exit
         (define pw+v (queue-remove! (channel-put-queue ch)))
         (define gw (current-thread/in-racket))
         (cond
           [(not pw+v)
            (define gq (channel-get-queue ch))
            (define n (queue-add! gq (cons gw b)))
            (waiter-suspend! gw
                             ;; On break/kill/suspend:
                             (lambda ()
                               (queue-remove-node! gq n)
                               ;; On retry after break or resume:
                               (lambda () (receive))))]
           [else
            (set-box! b (cdr pw+v))
            (waiter-resume! (car pw+v) (void))
            void]))))
     (unbox b)]))

;; in atomic mode
;; for cooperation with `port-commit-peeked`
(define (channel-get-poll-or-semaphore ch)
  (define-values (results sema) (channel-get/poll ch 'sema))
  (or results sema))

;; in atomic mode
(define (channel-get/poll ch poll-ctx)
  ;; Similar to `channel-get`, but works in terms of a
  ;; `select-waiter` instead of a thread
  (assert-atomic-mode)
  (define pq (channel-put-queue ch))
  (define pw+v (queue-fremove! pq not-matching-select-waiter))
  (cond
   [pw+v
    (waiter-resume! (car pw+v) (void))
    (values (list (cdr pw+v)) #f)]
   [(eq? poll-ctx 'sema)
    (unless (channel-put-queued-sema ch)
      (set-channel-put-queued-sema! ch (make-semaphore)))
    (values #f (channel-put-queued-sema ch))]
   [(poll-ctx-poll? poll-ctx)
    (values #f ch)]
   [else
    (define b (box #f))
    (define gq (channel-get-queue ch))
    (define gw (channel-select-waiter (poll-ctx-select-proc poll-ctx)
                                      (current-thread/in-racket)))
    (define n (queue-add! gq (cons gw b)))
    (define (post-queued)
      (define sema (channel-get-queued-sema ch))
      (when sema
        (semaphore-post-all/atomic sema)
        (set-channel-get-queued-sema! ch #f)))
    (post-queued)
    (values #f
            (control-state-evt async-evt
                               (lambda (v) (unbox b))
                               (lambda () (queue-remove-node! gq n))
                               void
                               (lambda ()
                                 ;; Retry: get ready value or requeue
                                 (define pw+v (queue-fremove! pq not-matching-select-waiter))
                                 (cond
                                   [pw+v
                                    (waiter-resume! (car pw+v) (void))
                                    (set-box! b (cdr pw+v))
                                    (values #t #t)]
                                   [else
                                    (set! n (queue-add! gq (cons gw b)))
                                    (post-queued)
                                    (values #f #f)]))))]))

;; ----------------------------------------

(define/who (channel-put ch v)
  (check who channel? ch)
  (cond
    [(channel-put-impersonator? ch)
     (channel-impersonator-put ch v channel-put)]
    [else
     ((atomically/no-barrier-exit
       (define gw+b (queue-remove! (channel-get-queue ch)))
       (define pw (current-thread/in-racket))
       (cond
         [(not gw+b)
          (define pq (channel-put-queue ch))
          (define n (queue-add! pq (cons pw v)))
          (waiter-suspend! pw
                           ;; On break/kill/suspend:
                           (lambda ()
                             (queue-remove-node! pq n)
                             ;; On retry after break or resume:
                             (lambda () (channel-put ch v))))]
         [else
          (set-box! (cdr gw+b) v)
          (waiter-resume! (car gw+b) v)
          void])))]))

;; in atomic mode
;; for cooperation with `port-commit-peeked`
(define (channel-put-poll-or-semaphore put-evt)
  (define ch (channel-put-evt*-ch put-evt))
  (define v (channel-put-evt*-v put-evt))
  (define-values (results sema) (channel-put/poll ch v put-evt 'sema))
  (or results sema))

;; In atomic mode
(define (channel-put/poll ch v self poll-ctx)
  ;; Similar to `channel-put`, but works in terms of a
  ;; `select-waiter` instead of a thread
  (assert-atomic-mode)
  (define gq (channel-get-queue ch))
  (define gw+b (queue-fremove! gq not-matching-select-waiter))
  (cond
   [gw+b
    (set-box! (cdr gw+b) v)
    (waiter-resume! (car gw+b) v)
    (values (list self) #f)]
   [(eq? poll-ctx 'sema)
    (unless (channel-get-queued-sema ch)
      (set-channel-get-queued-sema! ch (make-semaphore)))
    (values #f (channel-get-queued-sema ch))]
   [(poll-ctx-poll? poll-ctx)
    (values #f self)]
   [else
    (define pq (channel-put-queue ch))
    (define pw (channel-select-waiter (poll-ctx-select-proc poll-ctx)
                                      (current-thread/in-racket)))
    (define n (queue-add! pq (cons pw v)))
    (define (post-queued)
      (define sema (channel-put-queued-sema ch))
      (when sema
        (semaphore-post-all/atomic sema)
        (set-channel-put-queued-sema! ch #f)))
    (post-queued)
    (values #f
            (control-state-evt async-evt
                               (lambda (v) self)
                               (lambda () (queue-remove-node! pq n))
                               void
                               (lambda ()
                                 ;; Retry: put ready value or requeue
                                 (define gw+b (queue-fremove! gq not-matching-select-waiter))
                                 (cond
                                   [gw+b
                                    (set-box! (cdr gw+b) v)
                                    (waiter-resume! (car gw+b) v)
                                    (values self #t)]
                                   [else
                                    (set! n (queue-add! pq (cons pw v)))
                                    (post-queued)
                                    (values #f #f)]))))]))

(define/who (channel-put-evt ch v)
  (check who channel? ch)
  (cond
    [(channel-put-impersonator? ch)
     (channel-impersonator-put ch v channel-put-evt)]
    [else
     (channel-put-evt* ch v)]))

(define (channel-put-evt? v)
  (channel-put-evt*? v))

(define (channel-impersonator-put ch v channel-put)
  (define ch+put-proc (channel-put-impersonator-ref ch))
  (define old-ch (car ch+put-proc))
  (define new-v ((cdr ch+put-proc) old-ch v))
  (channel-put old-ch new-v))

(define (channel-put-do v)
  (channel-put (channel-put-evt*-ch v)
               (channel-put-evt*-v v)))

;; ----------------------------------------

;; in atomic mode
(define (not-matching-select-waiter w+b/v)
  (define w (car w+b/v))
  (or (not (channel-select-waiter? w))
      (not (eq? (current-thread/in-racket)
                (channel-select-waiter-thread w)))))

;; ----------------------------------------

;; To resolve a mutual dependency:
(define sync-on-channel #f)
(define (set-sync-on-channel! sync)
  (set! sync-on-channel sync))

;; ----------------------------------------

(define-values (impersonator-prop:channel-put channel-put-impersonator? channel-put-impersonator-ref)
  (make-impersonator-property 'channel-put-impersonator))
