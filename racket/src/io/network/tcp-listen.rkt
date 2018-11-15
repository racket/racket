#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../security/main.rkt"
         "../sandman/main.rkt"
         "port-number.rkt"
         "address.rkt"
         "error.rkt")

(provide tcp-listen
         tcp-listener?
         tcp-close

         tcp-listener-lnr
         tcp-listener-closed?)

(struct tcp-listener (lnr
                      closed ; boxed boolean
                      custodian-reference)
  #:authentic
  #:property prop:evt (poller (lambda (l ctx) (poll-listener l ctx))))

(define/who (tcp-listen port-no [max-allow-wait 4] [reuse? #f] [hostname #f])
  (check who listen-port-number?  port-no)
  (check who exact-nonnegative-integer? max-allow-wait)
  (check who string? #:or-false hostname)
  (define (raise-listen-error what err)
    (end-atomic)
    (raise-network-error who err
                         (string-append what
                                        (if hostname
                                            (format "\n  hostname: ~a" hostname)
                                            "")
                                        (format "\n  port number: ~a" port-no))))
  (security-guard-check-network who hostname port-no 'server)
  (let loop ([family RKTIO_FAMILY_ANY])
    ((atomically
      ;; Result is a thunk that might call `loop`
      ;; or might return a listener
      (call-with-resolved-address
       hostname port-no
       ;; in atomic mode
       (lambda (addr)
         (cond
           [(rktio-error? addr)
            (raise-listen-error "address-resolution error" addr)]
           [else
            (check-current-custodian who)
            (define lnr (rktio_listen rktio addr (min max-allow-wait 10000) reuse?))
            (cond
              [(rktio-error? lnr)
               (cond
                 [(racket-error? lnr RKTIO_ERROR_TRY_AGAIN_WITH_IPV4)
                  (lambda () (loop (rktio_get_ipv4_family rktio)))]
                 [else
                  (raise-listen-error "listen failed" lnr)])]
              [else
               (define closed (box #f))
               (define custodian-reference
                 (unsafe-custodian-register (current-custodian)
                                            lnr
                                            ;; in atomic mode
                                            (lambda (fd) (do-tcp-close lnr closed))
                                            #f
                                            #f))
               (lambda ()
                 (tcp-listener lnr closed custodian-reference))])])))))))

; in atomic mode
(define (do-tcp-close lnr closed)
  (rktio_listen_stop rktio lnr)
  (set-box! closed #t))

(define/who (tcp-close listener)
  (check who tcp-listener? listener)
  (define closed (tcp-listener-closed listener))
  (start-atomic)
  (cond
    [(unbox closed)
     (end-atomic)
     (raise-arguments-error who
                            "listener is closed"
                            "listener" listener)]
    [else
     (define lnr (tcp-listener-lnr listener))
     (do-tcp-close lnr closed)
     (unsafe-custodian-unregister lnr (tcp-listener-custodian-reference listener))
     (end-atomic)]))

;; in atomic mode
(define (tcp-listener-closed? listener)
  (unbox (tcp-listener-closed listener)))

;; ----------------------------------------

;; in atomic mode
(define (poll-listener l ctx)
  (cond
    [(unbox (tcp-listener-closed l))
     (values (list l) #f)]
    [(eqv? (rktio_poll_accept_ready rktio (tcp-listener-lnr l))
           RKTIO_POLL_READY)
     (values (list l) #f)]
    [else
     (sandman-poll-ctx-add-poll-set-adder!
      ctx
      (lambda (ps)
        (rktio_poll_add_accept rktio (tcp-listener-lnr l) ps)))
     (values #f l)]))
