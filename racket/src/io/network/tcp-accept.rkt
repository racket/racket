#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../sandman/main.rkt"
         "tcp-listen.rkt"
         "tcp-port.rkt"
         "evt.rkt"
         "error.rkt")

(provide tcp-accept
         tcp-accept/enable-break
         tcp-accept-evt
         tcp-accept-ready?)

(define/who (tcp-accept listener)
  (do-tcp-accept who listener))

(define/who (tcp-accept/enable-break listener)
  (do-tcp-accept who #:enable-break? #t listener))

(define (do-tcp-accept who listener
                       #:enable-break? [enable-break? #f])
  (check who tcp-listener? listener)
  (let loop ()
    (start-atomic)
    (cond
      [(tcp-listener-closed? listener)
       (closed-error who listener)]
      [(accept-ready? listener)
       (check-current-custodian who)
       (define fd (rktio_accept rktio (tcp-listener-lnr listener)))
       (cond
         [(rktio-error? fd)
          (end-atomic)
          (raise-network-error who fd "accept from listener failed")]
         [else
          (begin0
            (open-input-output-accetped-tcp fd)
            (end-atomic))])]
      [else
       (end-atomic)
       (sync (rktio-evt
              ;; in atomic mode
              (lambda ()
                (or (tcp-listener-closed? listener)
                    (accept-ready? listener)))
              ;; in atomic mode
              (lambda (ps)
                (rktio_poll_add_accept rktio (tcp-listener-lnr listener) ps))))
       (loop)])))

(define/who (tcp-accept-ready? listener)
  (check who tcp-listener? listener)
  (start-atomic)
  (cond
    [(tcp-listener-closed? listener)
     (closed-error who listener)]
    [else (accept-ready? listener)]))

;; ----------------------------------------

(define/who (tcp-accept-evt listener)
  (check who tcp-listener? listener)
  (accept-evt listener))

(struct accept-evt (listener)
  #:property
  prop:evt
  (poller
   ;; in atomic mode
   (lambda (self poll-ctx)
     (define listener (accept-evt-listener self))
     (cond
       [(tcp-listener-closed? listener)
        (error-result (lambda ()
                        (start-atomic)
                        (closed-error 'tcp-accept-evt listener)))]
       [(custodian-shut-down? (current-custodian))
        (let ([c (current-custodian)])
          (error-result (lambda ()
                          (start-atomic)
                          (parameterize ([current-custodian c])
                            (check-current-custodian 'tcp-accept-evt)))))]
       [(accept-ready? listener)
        (define fd (rktio_accept rktio (tcp-listener-lnr listener)))
        (cond
          [(rktio-error? fd)
           (end-atomic)
           (error-result (lambda ()
                           (raise-network-error 'tcp-accept-evt fd "accept from listener failed")))]
          [else
           (values (list (call-with-values (lambda () (open-input-output-accetped-tcp fd))
                           list))
                   #f)])]
       [else
        (define sched-info (poll-ctx-sched-info poll-ctx))
        (when sched-info
          (schedule-info-current-exts sched-info
                                      (sandman-add-poll-set-adder
                                       (schedule-info-current-exts sched-info)
                                       (lambda (ps)
                                         (rktio_poll_add_accept rktio (tcp-listener-lnr listener) ps)))))
        (values #f self)])))
  #:reflection-name 'tcp-accept-evt)

(define (error-result thunk)
  (values #f
          (wrap-evt always-evt (lambda (v) (thunk)))))

;; ----------------------------------------

;; in atomic mode
;; assumes that listener is not closed
(define (accept-ready? listener)
  (not (eqv? (rktio_poll_accept_ready rktio (tcp-listener-lnr listener))
             RKTIO_POLL_NOT_READY)))

;; in atomic mode
(define (closed-error who listener)
  (end-atomic)
  (raise-arguments-error who
                         "listener is closed"
                         "listener" listener))

;; in atomic mode
(define (open-input-output-accetped-tcp fd)
  (open-input-output-tcp fd "tcp-accepted"))
