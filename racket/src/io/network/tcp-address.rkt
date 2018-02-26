#lang racket/base
(require "../common/check.rkt"
         "../string/convert.rkt"
         "../string/integer.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../port/close.rkt"
         "../port/fd-port.rkt"
         "tcp-port.rkt"
         "tcp-listen.rkt"
         "udp-socket.rkt"
         "error.rkt")

(provide tcp-addresses)

(define/who (tcp-addresses p [port-numbers? #f])
  (check who (lambda (p) (or (tcp-port? p) (tcp-listener? p) (udp? p)))
         #:contract "(or/c tcp-port? tcp-listener? udp?)"
         p)
  (start-atomic)
  (define-values (local-address peer-address)
    (cond
      [(tcp-listener? p)
       (cond
         [(tcp-listener-closed? p)
          (end-atomic)
          (raise-arguments-error who
                                 "listener is closed"
                                 "listener" p)]
         [else
          (values (rktio_listener_address rktio (tcp-listener-lnr p))
                  #f)])]
      [else
       (define fd
         (cond
           [(udp? p)
            (check-udp-closed who p)
            (udp-s p)]
           [(port-closed? p)
            (end-atomic)
            (raise-arguments-error who
                                   "port is closed"
                                   "port" p)]
           [else (fd-port-fd p)]))
       (values (rktio_socket_address rktio fd)
               (rktio_socket_peer_address rktio fd))]))
  (define local-address-bytes (and (not (rktio-error? local-address))
                                   (rktio_to_bytes_list local-address 2)))
  (define peer-address-bytes (and peer-address
                                  (not (rktio-error? peer-address))
                                  (rktio_to_bytes_list peer-address 2)))
  (end-atomic)

  (when (rktio-error? local-address)
    (raise-network-error who local-address "could not get address"))
  (when (and (rktio-error? peer-address)
             ;; It's ok for the peer-address request to fail for UDP sockets
             (not (udp? p)))
    (raise-network-error who peer-address "could not get peer address"))

  (define (convert bstr) (bytes->string/utf-8 bstr #\?))
  (define local-hostname (convert (car local-address-bytes)))
  (define peer-hostname (if peer-address-bytes
                            (convert (car peer-address-bytes))
                            "0.0.0.0"))

  (cond
    [port-numbers?
     (values local-hostname
             (string->integer (convert (cadr local-address-bytes)))
             peer-hostname
             (if peer-address-bytes
                 (string->integer (convert (cadr peer-address-bytes)))
                 0))]
    [else
     (values local-hostname peer-hostname)]))
