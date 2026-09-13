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
    (start-rktio)
    (cond
      [(tcp-listener-closed? listener)
       (closed-error who listener)]
      [(accept-ready? listener)
       (unsafe-uninterruptible-custodian-lock-acquire)
       (check-current-custodian who #:unlock (lambda ()
                                               (unsafe-uninterruptible-custodian-lock-release)
                                               (end-rktio)))
       (define fd (rktio_accept rktio (tcp-listener-lnr listener)))
       (cond
         [(rktio-error? fd)
          (end-rktio)
          (unsafe-uninterruptible-custodian-lock-acquire)
          (raise-network-error who fd "accept from listener failed")]
         [else
          (begin0
            (open-input-output-accepted-tcp fd)
            (unsafe-uninterruptible-custodian-lock-release)
            (end-rktio))])]
      [else
       (end-rktio)
       ((if enable-break? sync/enable-break sync)
        (rktio-evt
         ;; in atomic mode
         (lambda ()
           (rktioly
            (or (tcp-listener-closed? listener)
                (accept-ready? listener))))
         ;; in atomic and in rktio-sleep-relevant, must not start nested rktio
         (lambda (ps)
           (rktio_poll_add_accept rktio (tcp-listener-lnr listener) ps))))
       (loop)])))

(define/who (tcp-accept-ready? listener)
  (check who tcp-listener? listener)
  (start-rktio)
  (cond
    [(tcp-listener-closed? listener)
     (closed-error who listener)]
    [else
     (begin0
       (accept-ready? listener)
       (end-rktio))]))

;; ----------------------------------------

(define/who (tcp-accept-evt listener)
  (check who tcp-listener? listener)
  ;; will have to check the custodian again later, but catch early errors:
  (check-current-custodian 'tcp-accept-evt #:unlock void)
  (accept-evt listener (current-custodian)))

(struct accept-evt (listener custodian)
  #:property
  prop:evt
  (poller
   ;; in atomic mode
   (lambda (self poll-ctx)
     (define listener (accept-evt-listener self))
     (define custodian (accept-evt-custodian self))
     (cond
       [(tcp-listener-closed? listener)
        (error-result (lambda () ; out of atomic mode
                        (start-rktio)
                        (closed-error 'tcp-accept-evt listener)))]
       [(custodian-shut-down? custodian)
        (let ([c (accept-evt-listener self)])
          (error-result (lambda () ; out of atomic mode
                          (parameterize ([current-custodian custodian])
                            (check-current-custodian 'tcp-accept-evt #:unlock void)))))]
       [else
        (start-rktio)
        (cond
          [(accept-ready? listener)
           (define fd (rktio_accept rktio (tcp-listener-lnr listener)))
           (cond
             [(rktio-error? fd)
              (end-rktio)
              (error-result (lambda () ; out of atomic mode
                              (raise-network-error 'tcp-accept-evt fd "accept from listener failed")))]
             [else
              (define results (call-with-values (lambda () (open-input-output-accepted-tcp fd #:custodian custodian))
                                                list))
              (end-rktio)
              (values (list results)
                      #f)])]
          [else
           (end-rktio)
           (define sched-info (poll-ctx-sched-info poll-ctx))
           (when sched-info
             (schedule-info-current-exts sched-info
                                         (sandman-add-poll-set-adder
                                          (schedule-info-current-exts sched-info)
                                          ;; in atomic and in rktio-sleep-relevant, must not start nested rktio
                                          (lambda (ps)
                                            (rktio_poll_add_accept rktio (tcp-listener-lnr listener) ps)))))
           (values #f self)])])))
  #:reflection-name 'tcp-accept-evt)

(define (error-result thunk)
  (values #f
          (wrap-evt always-evt (lambda (v) (thunk)))))

;; ----------------------------------------

;; in rktio mode
;; assumes that listener is not closed
(define (accept-ready? listener)
  (not (eqv? (rktio_poll_accept_ready rktio (tcp-listener-lnr listener))
             RKTIO_POLL_NOT_READY)))

;; in rktio mode
(define (closed-error who listener)
  (end-rktio)
  (raise-arguments-error who
                         "listener is closed"
                         "listener" listener))

;; in rktio mode
(define (open-input-output-accepted-tcp fd #:custodian [custodian (current-custodian)])
  (rktio_tcp_nodelay rktio fd #t) ; initially block buffered
  (rktio_tcp_keepalive rktio fd #t)
  (open-input-output-tcp fd "tcp-accepted" #:custodian custodian))
