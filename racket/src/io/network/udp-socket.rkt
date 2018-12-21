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

         check-udp-closed
         handle-error-immediately
         udp-default-family

         udp-s
         set-udp-is-bound?!
         set-udp-is-connected?!)

(struct udp (s is-bound? is-connected?)
  #:mutable
  #:authentic)

(define/who (udp-open-socket [family-hostname #f] [family-port-no #f])
  (check who string? #:or-false family-hostname)
  (check who port-number? #:or-false family-port-no)
  (security-guard-check-network who family-hostname family-port-no 'server)
  (atomically
   (call-with-resolved-address
    #:who who
    family-hostname family-port-no
    #:tcp? #f
    (lambda (addr)
      (define s (rktio_udp_open rktio addr (udp-default-family)))
      (cond
        [(rktio-error? s)
         (end-atomic)
         (raise-network-error who s "creation failed")]
        [else
         (udp s #f #f)])))))

(define/who (udp-close u)
  (check who udp? u)
  (atomically
   (cond
     [(udp-s u)
      (rktio_close rktio (udp-s u))
      (set-udp-s! u #f)]
     [else
      (end-atomic)
      (raise-network-arguments-error who "udp socket was already closed"
                                     "socket" u)])))

;; ----------------------------------------

(define/who (udp-bound? u)
  (check who udp? u)
  (udp-is-bound? u))

(define/who (udp-bind! u hostname port-no [reuse? #f])
  (check who udp? u)
  (check who string? #:or-false hostname)
  (check who listen-port-number? port-no)
  (security-guard-check-network who hostname port-no 'server)
  (atomically
   (call-with-resolved-address
    #:who who
    hostname port-no
    #:tcp? #f
    #:passive? #t
    (lambda (addr)
      (check-udp-closed who u)
      (when (udp-is-bound? u)
        (end-atomic)
        (raise-arguments-error who "udp socket is already bound"
                               "socket" u))
      (define b (rktio_udp_bind rktio (udp-s u) addr reuse?))
      (when (rktio-error? b)
        (end-atomic)
        (raise-network-error who b
                             (string-append "can't bind" (if reuse? " as reusable" "")
                                            "\n  address: " (or hostname "<unspec>")
                                            "\n  port number: " (number->string port-no))))
      (set-udp-is-bound?! u #t)))))

(define/who (udp-connected? u)
  (check who udp? u)
  (udp-is-connected? u))

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
  (atomically
   (cond
     [(not hostname)
      (check-udp-closed who u)
      (when (udp-is-connected? u)
        (define d (rktio_udp_disconnect rktio (udp-s u)))
        (when (rktio-error? d)
          (end-atomic)
          (raise-network-error who d "can't disconnect"))
        (set-udp-is-connected?! u #f))]
     [else
      (call-with-resolved-address
       #:who who
       hostname port-no
       #:tcp? #f
       (lambda (addr)
         (check-udp-closed who u)
         (define c (rktio_udp_connect rktio (udp-s u) addr))
         (when (rktio-error? c)
           (end-atomic)
           (raise-network-error who c
                                (string-append "can't connect"
                                               "\n  address: " hostname
                                               "\n  port number: " (number->string port-no))))
         (set-udp-is-connected?! u #t)))])))

;; ----------------------------------------

;; in atomic mode
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

(define (handle-error-immediately thunk)
  (end-atomic)
  (thunk))
  
(define (udp-default-family)
  (rktio_get_ipv4_family rktio))
