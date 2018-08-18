#lang racket/base
(require "check.rkt"
         "atomic.rkt"
         "parameter.rkt"
         "evt.rkt"
         "waiter.rkt"
         "../common/queue.rkt")

(provide make-channel
         channel?
         channel-put
         channel-get
         
         channel-put-evt
         channel-put-evt?
         channel-put-do)

(module+ for-sync
  (provide set-sync-on-channel!))

(module+ for-impersonator
  (provide impersonator-prop:channel-put
           channel-put-impersonator?
           channel-put-impersonator-ref))

;; ----------------------------------------

(struct channel (get-queue
                 put-queue)
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
  (channel (make-queue) (make-queue)))

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
       ((atomically
         (define pw+v (queue-remove! (channel-put-queue ch)))
         (define gw (current-thread))
         (cond
           [(not pw+v)
            (define gq (channel-get-queue ch))
            (define n (queue-add! gq (cons gw b)))
            (waiter-suspend! gw
                             ;; On break/kill/suspend:
                             (lambda () (queue-remove-node! gq n))
                             ;; On retry after break or resume:
                             (lambda () (receive)))]
           [else
            (set-box! b (cdr pw+v))
            (waiter-resume! (car pw+v) (void))
            void]))))
     (unbox b)]))

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
   [(poll-ctx-poll? poll-ctx)
    (values #f never-evt)]
   [else
    (define b (box #f))
    (define gq (channel-get-queue ch))
    (define gw (channel-select-waiter (poll-ctx-select-proc poll-ctx)
                                      (current-thread)))
    (define n (queue-add! gq (cons gw b)))
    (values #f
            (wrap-evt
             (control-state-evt async-evt
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
                                    (values #f #f)])))
             (lambda (v) (unbox b))))]))

;; ----------------------------------------


(define/who (channel-put ch v)
  (check who channel? ch)
  (cond
    [(channel-put-impersonator? ch)
     (channel-impersonator-put ch v channel-put)]
    [else
     ((atomically
       (define gw+b (queue-remove! (channel-get-queue ch)))
       (define pw (current-thread))
       (cond
         [(not gw+b)
          (define pq (channel-put-queue ch))
          (define n (queue-add! pq (cons pw v)))
          (waiter-suspend! pw
                           ;; On break/kill/suspend:
                           (lambda () (queue-remove-node! pq n))
                           ;; On retry after break or resume:
                           (lambda () (channel-put ch v)))]
         [else
          (set-box! (cdr gw+b) v)
          (waiter-resume! (car gw+b) v)
          void])))]))

;; In atomic mode
(define (channel-put/poll ch v result poll-ctx)
  ;; Similar to `channel-put`, but works in terms of a
  ;; `select-waiter` instead of a thread
  (assert-atomic-mode)
  (define gq (channel-get-queue ch))
  (define gw+b (queue-fremove! gq not-matching-select-waiter))
  (cond
   [gw+b
    (set-box! (cdr gw+b) v)
    (waiter-resume! (car gw+b) v)
    (values (list result) #f)]
   [(poll-ctx-poll? poll-ctx)
    (values #f async-evt)]
   [else
    (define pq (channel-put-queue ch))
    (define pw (channel-select-waiter (poll-ctx-select-proc poll-ctx)
                                      (current-thread)))
    (define n (queue-add! pq (cons pw v)))
    (values #f
            (wrap-evt
             (control-state-evt async-evt
                                (lambda () (queue-remove-node! pq n))
                                void
                                (lambda ()
                                  ;; Retry: put ready value or requeue
                                  (define gw+b (queue-fremove! gq not-matching-select-waiter))
                                  (cond
                                   [gw+b
                                    (set-box! (cdr gw+b) v)
                                    (waiter-resume! (car gw+b) v)
                                    (values result #t)]
                                   [else
                                    (set! n (queue-add! pq (cons pw v)))
                                    (values #f #f)])))
             (lambda (v) result)))]))

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

(define (not-matching-select-waiter w+b/v)
  (define w (car w+b/v))
  (or (not (channel-select-waiter? w))
      (not (eq? (current-thread)
                (channel-select-waiter-thread w)))))

;; ----------------------------------------

;; To resolve a mutual dependency:
(define sync-on-channel #f)
(define (set-sync-on-channel! sync)
  (set! sync-on-channel sync))

;; ----------------------------------------

(define-values (impersonator-prop:channel-put channel-put-impersonator? channel-put-impersonator-ref)
  (make-impersonator-property 'channel-put-impersonator))
