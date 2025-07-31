#lang racket/base
(require "../common/check.rkt"
         "../common/resource.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../security/main.rkt"
         "../format/main.rkt"
         "../sandman/ltps.rkt"
         "tcp-port.rkt"
         "port-number.rkt"
         "address.rkt"
         "evt.rkt"
         "error.rkt")

(provide tcp-connect
         tcp-connect/enable-break)

(struct connect-progress (conn trying-fd)
  #:mutable
  #:authentic)

(define/who (tcp-connect hostname port-no [local-hostname #f] [local-port-no #f])
  (do-tcp-connect who hostname port-no local-hostname local-port-no))

(define/who (tcp-connect/enable-break hostname port-no [local-hostname #f] [local-port-no #f])
  (do-tcp-connect who #:enable-break? #t hostname port-no local-hostname local-port-no))

(define (do-tcp-connect who hostname port-no [local-hostname #f] [local-port-no #f]
                        #:enable-break? [enable-break? #f])
  (check who string? hostname)
  (check who port-number? port-no)
  (check who string? #:or-false local-hostname)
  (check who port-number? #:or-false local-port-no)
  (when (and local-hostname (not local-port-no))
    (raise-arguments-error who
                           "no local port number supplied when local hostname was supplied"
                           "hostname" local-hostname))
  ;; in uninterruptible mode, *not* rktio mode (but exits uninterruptible to raise an exception)
  (define (raise-connect-error err
                               [what "connection failed"]
                               [hostname hostname]
                               [port-no port-no])
    (end-uninterruptible)
    (raise-network-error who err
                         (string-append what
                                        (if hostname
                                            (format "\n  hostname: ~a" hostname)
                                            "")
                                        (if port-no
                                            (format "\n  port number: ~a" port-no)
                                            ""))))
  (security-guard-check-network who hostname port-no 'client)
  (start-uninterruptible) ; because `call-with-resolved-address` and `call-with-resource`
  (define-values (in out)
    (call-with-resolved-address
     hostname port-no
     #:enable-break? enable-break?
     ;; in uninterruptible mode, *not* rktio mode
     (lambda (remote-addr)
       (cond
         [(rktio-error? remote-addr)
          (raise-connect-error remote-addr "host not found")]
         [else
          (call-with-resolved-address
           local-hostname local-port-no
           #:enable-break? enable-break?
           ;; in uninterruptible mode, *not* rktio mode
           (lambda (local-addr)
             (cond
               [(rktio-error? local-addr)
                (raise-connect-error local-addr "local host not found" local-hostname local-port-no)]
               [else
                (call-with-resource
                 (connect-progress (rktioly (rktio_start_connect rktio remote-addr local-addr))
                                   #f)
                 ;; in uninterruptible mode (possibly atomic), *not* rktio mode
                 (lambda (conn-prog)
                   (start-rktio)
                   (remove-trying-fd! conn-prog)
                   (define conn (connect-progress-conn conn-prog))
                   (when conn
                     (rktio_connect_stop rktio conn))
                   (end-rktio))
                 ;; in uninterruptible mode, *not* rktio mode
                 (lambda (conn-prog)
                   (define conn (connect-progress-conn conn-prog))
                   (cond
                     [(rktio-error? conn)
                      (raise-connect-error conn)]
                     [else
                      (let loop ()
                        (start-rktio)
                        (cond
                          [(eqv? (rktio_poll_connect_ready rktio conn)
                                 RKTIO_POLL_NOT_READY)
                           (init-trying-fd! conn-prog)
                           (end-rktio)
                           (end-uninterruptible)
                           ((if enable-break? sync/enable-break sync)
                            (rktio-evt (lambda ()
                                         (not (eqv? (rktio_poll_connect_ready rktio conn)
                                                    RKTIO_POLL_NOT_READY)))
                                       ;; in atomic and in rktio-sleep-relevant, must not start nested rktio
                                       (lambda (ps)
                                         (rktio_poll_add_connect rktio conn ps))))
                           (start-uninterruptible)
                           (loop)]
                          [else
                           (remove-trying-fd! conn-prog)
                           (unsafe-uninterruptible-custodian-lock-acquire)
                           (check-current-custodian who #:unlock (lambda ()
                                                                   (unsafe-uninterruptible-custodian-lock-release)
                                                                   (end-rktio+uninterruptible)))
                           (define fd (rktio_connect_finish rktio conn))
                           (cond
                             [(rktio-error? fd)
                              (unsafe-uninterruptible-custodian-lock-release)
                              (end-rktio)
                              (cond
                                [(racket-error? fd RKTIO_ERROR_CONNECT_TRYING_NEXT)
                                 (loop)]
                                [else
                                 ;; other errors imply that `conn` is destroyed
                                 (set-connect-progress-conn! conn-prog #f)
                                 (raise-connect-error fd)])]
                             [else
                              (define name (string->immutable-string hostname))
                              (rktio_tcp_nodelay rktio fd #t) ; initially block buffered
                              (rktio_tcp_keepalive rktio fd #t)
                              (end-rktio)
                              (define-values (in out) (open-input-output-tcp fd name))
                              (unsafe-uninterruptible-custodian-lock-release)
                              (values in out)])]))])))])))]))))
  (end-uninterruptible)
  (values in out))

;; in uninterruptible and rktio mode
(define (init-trying-fd! conn-prog)
  (unless (connect-progress-trying-fd conn-prog)
    ;; Even though we don't use the semaphore for the registered file
    ;; descriptor, registering it seems to avoid a problem where
    ;; `rktio_poll_add_connect` doesn't work, at least on Mac OS.
    (define fd (rktio_connect_trying rktio (connect-progress-conn conn-prog)))
    (set-connect-progress-trying-fd! conn-prog fd)
    (fd-semaphore-update! fd 'write)
    (void)))

;; in uninterruptible mode and rktio mode
(define (remove-trying-fd! conn-prog)
  (define fd (connect-progress-trying-fd conn-prog))
  (when fd
    (fd-semaphore-update! fd 'remove)
    (set-connect-progress-trying-fd! conn-prog #f)))
