#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../security/main.rkt"
         "port-number.rkt"
         "address.rkt"
         "error.rkt")

(provide udp?
         udp-open-socket
         udp-close

         udp-bound?
         udp-connected?

         udp-bind!
         udp-connect!

         udp-ttl
         udp-set-ttl!

         check-udp-closed
         check-udp-closed*
         handle-error-immediately
         handle-error-immediately*
         udp-default-family

         udp-s
         udp-is-bound?
         udp-is-connected?
         set-udp-is-bound?!
         set-udp-is-connected?!)

;; a udp record is locked by `start-rktio`/`end-rktio`
;; and closing is further guarded by rktio-sleep-relevant mode
(struct udp (s-box is-bound? is-connected? custodian-reference)
  #:mutable
  #:authentic)

;; in rktio mode
(define (udp-s u) (unbox (udp-s-box u)))

(define/who (udp-open-socket [family-hostname #f] [family-port-no #f])
  (check who string? #:or-false family-hostname)
  (check who port-number? #:or-false family-port-no)
  (security-guard-check-network who family-hostname family-port-no 'server)
  (start-uninterruptible) ; because `call-with-resolved-address`
  (define u
    (call-with-resolved-address
     #:who who
     family-hostname family-port-no
     #:tcp? #f
     ;; in uninteruptible mode, *not* rktio mode
     (lambda (addr)
       (start-rktio)
       (unsafe-uninterruptible-custodian-lock-acquire)
       (check-current-custodian who #:unlock (lambda ()
                                               (unsafe-uninterruptible-custodian-lock-release)
                                               (end-rktio)
                                               (end-uninterruptible)))
       (define s (rktio_udp_open rktio addr (udp-default-family)))
       (end-rktio)
       (cond
         [(rktio-error? s)
          (unsafe-uninterruptible-custodian-lock-release)
          (end-uninterruptible)
          (raise-network-error who s "creation failed")]
         [else
          (define s-box (box s))
          (define custodian-reference
            (unsafe-custodian-register (current-custodian)
                                       s-box
                                       ;; in atomic mode
                                       (lambda (s-box) (rktioly (do-udp-close s-box)))
                                       #f
                                       #f))
          (unsafe-uninterruptible-custodian-lock-release)
          (udp s-box #f #f custodian-reference)]))))
  (end-uninterruptible)
  u)

; in rktio mode
(define (do-udp-close s-box)
  (start-rktio-sleep-relevant)
  (define s (unbox s-box))
  (when s
    (rktio_close rktio s)
    (set-box! s-box #f))
  (end-rktio-sleep-relevant))

;; for external use, so *not* in rktio mode
(define/who (udp-close u)
  (check who udp? u)
  (rktioly
   (cond
     [(udp-s u)
      (define s-box (udp-s-box u))
      (do-udp-close s-box)
      (unsafe-custodian-unregister s-box (udp-custodian-reference u))]
     [else
      (end-rktio)
      (raise-network-arguments-error who "udp socket was already closed"
                                     "socket" u)])))

;; ----------------------------------------

;; for external use, so *not* in rktio mode
(define/who (udp-bound? u)
  (check who udp? u)
  (rktioly (udp-is-bound? u)))

(define/who (udp-bind! u hostname port-no [reuse? #f])
  (check who udp? u)
  (check who string? #:or-false hostname)
  (check who listen-port-number? port-no)
  (security-guard-check-network who hostname port-no 'server)
  (start-uninterruptible) ; because `call-with-resolved-address`
  (call-with-resolved-address
   #:who who
   hostname port-no
   #:tcp? #f
   #:passive? #t
   (lambda (addr)
     (start-rktio)
     (check-udp-closed* who u)
     (when (udp-is-bound? u)
       (end-rktio)
       (end-uninterruptible)
       (raise-arguments-error who "udp socket is already bound"
                              "socket" u))
     (define b (rktio_udp_bind rktio (udp-s u) addr reuse?))
     (when (rktio-error? b)
       (end-rktio)
       (end-uninterruptible)
       (raise-network-error who b
                            (string-append "can't bind" (if reuse? " as reusable" "")
                                           "\n  address: " (or hostname "<unspec>")
                                           "\n  port number: " (number->string port-no))))
     (set-udp-is-bound?! u #t)
     (end-rktio)))
  (end-uninterruptible))

;; *not* in rktio mode
(define/who (udp-connected? u)
  (check who udp? u)
  (rktioly (udp-is-connected? u)))

(define/who (udp-connect! u hostname port-no)
  (check who udp? u)
  (check who string? #:or-false hostname)
  (check who port-number? #:or-false port-no)
  (unless (eq? (not hostname) (not port-no))
    (raise-arguments-error who
                           "last second and third arguments must be both #f or both non-#f"
                           "second argument" hostname
                           "third argument" port-no))
  (security-guard-check-network who hostname port-no 'client)
  (start-uninterruptible) ; because `call-with-resolved-address`
  (cond
    [(not hostname)
     (start-rktio)
     (check-udp-closed* who u)
     (when (udp-is-connected? u)
       (define d (rktio_udp_disconnect rktio (udp-s u)))
       (when (rktio-error? d)
         (end-rktio)
         (end-uninterruptible)
         (raise-network-error who d "can't disconnect"))
       (set-udp-is-connected?! u #f))
     (end-rktio)]
    [else
     (call-with-resolved-address
      #:who who
      hostname port-no
      #:tcp? #f
      (lambda (addr)
        (start-rktio)
        (check-udp-closed* who u)
        (define c (rktio_udp_connect rktio (udp-s u) addr))
        (when (rktio-error? c)
          (end-rktio)
          (end-uninterruptible)
          (raise-network-error who c
                               (string-append "can't connect"
                                              "\n  address: " hostname
                                              "\n  port number: " (number->string port-no))))
        (set-udp-is-connected?! u #t)
        (end-rktio)))])
  (end-uninterruptible))

;; ----------------------------------------

;; in rktio mode, and maybe more as determined by `handle-error`
(define (check-udp-closed who u
                          #:handle-error [handle-error handle-error-immediately]
                          #:continue [continue void])
  (cond
    [(udp-s u) (continue)]
    [else
     (handle-error
      (lambda ()
        (raise-network-arguments-error who "udp socket is closed"
                                       "socket" u)))]))

;; in uninterruptible mode and rktio mode
(define (check-udp-closed* who u)
  (check-udp-closed who u
                    #:handle-error handle-error-immediately*))

;; in rktio mode
(define (handle-error-immediately thunk)
  (end-rktio)
  (thunk))

;; in uninterrupted mode and rktio mode
(define (handle-error-immediately* thunk)
  (end-rktio)
  (end-uninterruptible)
  (thunk))
  
(define (udp-default-family)
  (rktio_get_ipv4_family rktio))

;; ----------------------------------------

;; for external, so *not* in rktio mode
(define/who (udp-ttl u)
  (check who udp? u)
  (rktioly
   (check-udp-closed who u)
   (define v (rktio_udp_get_ttl rktio (udp-s u)))
   (cond
     [(rktio-error? v)
      (end-rktio)
      (raise-network-option-error who "get" v)]
     [else v])))

;; for external, so *not* in rktio mode
(define/who (udp-set-ttl! u ttl)
  (check who udp? u)
  (check who byte? ttl)
  (rktioly
   (check-udp-closed who u)
   (define r (rktio_udp_set_ttl rktio (udp-s u) ttl))
   (when (rktio-error? r)
     (end-rktio)
     (raise-network-option-error who "set" r))))
