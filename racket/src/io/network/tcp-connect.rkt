#lang racket/base
(require "../common/check.rkt"
         "../common/resource.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../host/error.rkt"
         "../security/main.rkt"
         "../format/main.rkt"
         "tcp-port.rkt"
         "port-number.rkt"
         "address.rkt"
         "evt.rkt"
         "error.rkt")

(provide tcp-connect
         tcp-connect/enable-break)

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
  ;; in atomic mode (but exits atomic mode to raise an exception)
  (define (raise-connect-error err
                               [what "connection failed"]
                               [hostname hostname]
                               [port-no port-no])
    (end-atomic)
    (raise-network-error who err
                         (string-append what
                                        (if hostname
                                            (format "\n  hostname: ~a" hostname)
                                            "")
                                        (if port-no
                                            (format "\n  port number: ~a" port-no)
                                            ""))))
  (security-guard-check-network who hostname port-no 'client)
  (atomically
   (call-with-resolved-address
    hostname port-no
    #:enable-break? enable-break?
    ;; in atomic mode
    (lambda (remote-addr)
      (cond
        [(rktio-error? remote-addr)
         (raise-connect-error remote-addr "host not found")]
        [else
         (call-with-resolved-address
          local-hostname local-port-no
          #:enable-break? enable-break?
          ;; in atomic mode
          (lambda (local-addr)
            (cond
              [(rktio-error? local-addr)
               (raise-connect-error local-addr "local host not found" local-hostname local-port-no)]
              [else
               (call-with-resource
                (box (rktio_start_connect rktio remote-addr local-addr))
                ;; in atomic mode
                (lambda (conn-box)
                  (define conn (unbox conn-box))
                  (when conn
                    (rktio_connect_stop rktio conn)))
                ;; in atomic mode
                (lambda (conn-box)
                  (define conn (unbox conn-box))
                  (cond
                    [(rktio-error? conn)
                     (raise-connect-error conn)]
                    [else
                     (let loop ()
                       (cond
                         [(eqv? (rktio_poll_connect_ready rktio conn)
                                RKTIO_POLL_NOT_READY)
                          (end-atomic)
                          ((if enable-break? sync/enable-break sync)
                           (rktio-evt (lambda ()
                                        (not (eqv? (rktio_poll_connect_ready rktio conn)
                                                   RKTIO_POLL_NOT_READY)))
                                      (lambda (ps)
                                        (rktio_poll_add_connect rktio conn ps))))
                          (start-atomic)
                          (loop)]
                         [else
                          (check-current-custodian who)
                          (define fd (rktio_connect_finish rktio conn))
                          (cond
                            [(rktio-error? fd)
                             (cond
                               [(racket-error? fd RKTIO_ERROR_CONNECT_TRYING_NEXT)
                                (loop)]
                               [else
                                ;; other errors imply that `conn` is destroyed
                                (set-box! conn-box #f)
                                (raise-connect-error fd)])]
                            [else
                             (define name (string->immutable-string hostname))
                             (open-input-output-tcp fd name)])]))])))])))])))))
