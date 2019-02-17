#lang racket/base
(require "../host/thread.rkt"
         "../host/rktio.rkt"
         "../sandman/main.rkt")

(provide unsafe-poller
         unsafe-poll-ctx-fd-wakeup
         unsafe-poll-ctx-eventmask-wakeup
         unsafe-poll-ctx-milliseconds-wakeup
         unsafe-signal-received
         unsafe-make-signal-received
         unsafe-set-sleep-in-thread!)

(define (unsafe-poller proc)
  (poller (lambda (self poll-ctx)
            (cond
              [(poll-ctx-poll? poll-ctx)
               (proc self #f)]
              [else
               (define-values (vals evt) (proc self #f))
               (cond
                 [vals (values vals #f)]
                 [(eq? evt self)
                  ;; Register wakeups:
                  (define-values (vals evt) (proc self poll-ctx))
                  (when vals
                    ;; The rule here is that we cancel any sleep so
                    ;; that the event will be polled again; we do not
                    ;; select the event now. That rule accomodates
                    ;; the old Racket scheduler.
                    (sandman-poll-ctx-merge-timeout poll-ctx (current-inexact-milliseconds)))
                  (values #f self)]
                 [else
                  (values #f evt)])]))))

(define (unsafe-poll-ctx-fd-wakeup poll-ctx fd mode)
  (when poll-ctx
    (sandman-poll-ctx-add-poll-set-adder! poll-ctx
                                          (lambda (ps)
                                            (atomically
                                             (define rfd (rktio_system_fd rktio
                                                                          fd
                                                                          (case mode
                                                                            [(read) RKTIO_OPEN_READ]
                                                                            [else RKTIO_OPEN_WRITE])))
                                             (rktio_poll_add rktio rfd ps (case mode
                                                                            [(read) RKTIO_POLL_READ]
                                                                            [else RKTIO_POLL_WRITE]))
                                             (rktio_forget rktio rfd))))))

(define (unsafe-poll-ctx-eventmask-wakeup poll-ctx event-mask)
  (when poll-ctx
    (sandman-poll-ctx-add-poll-set-adder! poll-ctx
                                          (lambda (ps)
                                            (rktio_poll_set_add_eventmask rktio ps event-mask)))))

(define (unsafe-poll-ctx-milliseconds-wakeup poll-ctx msecs)
  (when poll-ctx
    (sandman-poll-ctx-merge-timeout poll-ctx msecs)))
  
(define (unsafe-signal-received)
  (rktio_signal_received rktio))

(define (unsafe-make-signal-received)
  (let ([rktio rktio]) ; capture current place's `rktio`
    (lambda()
      (rktio_signal_received rktio))))

(define (unsafe-set-sleep-in-thread! do-sleep woke-fd)
  (sandman-set-background-sleep! do-sleep woke-fd))
