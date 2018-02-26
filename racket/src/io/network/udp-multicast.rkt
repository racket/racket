#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../string/convert.rkt"
         "udp-socket.rkt"
         "address.rkt"
         "error.rkt")

(provide udp-multicast-join-group!
         udp-multicast-leave-group!
         
         udp-multicast-interface
         udp-multicast-set-interface!
         
         udp-multicast-loopback?
         udp-multicast-set-loopback!
         
         udp-multicast-ttl
         udp-multicast-set-ttl!)

;; ----------------------------------------

(define/who (udp-multicast-join-group! u
                                       multicast-hostname
                                       hostname)
  (do-udp-multicast-join-or-leave-group! who
                                         RKTIO_ADD_MEMBERSHIP
                                         u	 	 	 	 
                                         multicast-hostname	 	 	 	 
                                         hostname))

(define/who (udp-multicast-leave-group! u
                                        multicast-hostname
                                        hostname)
  (do-udp-multicast-join-or-leave-group! who
                                         RKTIO_DROP_MEMBERSHIP
                                        u	 	 	 	 
                                        multicast-hostname	 	 	 	 
                                        hostname))

(define (do-udp-multicast-join-or-leave-group! who action u multicast-hostname hostname)
  (check who udp? u)
  (check who string? multicast-hostname)
  (check who string? #:or-false hostname)
  (atomically
   (call-with-resolved-address
    #:who who
    #:which "multicast "
    #:port-number-on-error? #f
    multicast-hostname -1
    #:family (udp-default-family)
    #:tcp? #f
    (lambda (multicast-addr)
      (call-with-resolved-address
       #:who who
       #:which "interface "
       #:port-number-on-error? #f
       hostname (and hostname -1)
       #:family (udp-default-family)
       #:tcp? #f
       (lambda (intf-addr)
         (check-udp-closed who u)
         (define v (rktio_udp_change_multicast_group rktio (udp-s u) multicast-addr intf-addr action))
         (when (rktio-error? v)
           (raise-option-error who "set" v))))))))

(define (raise-option-error who mode v)
  (end-atomic)
  (raise-network-error who v (string-append mode "sockopt failed")))

;; ----------------------------------------

(define/who (udp-multicast-interface u)
  (check who udp? u)
  (start-atomic)
  (check-udp-closed who u)
  (define v (rktio_udp_multicast_interface rktio (udp-s u)))
  (cond
    [(rktio-error? v)
     (raise-option-error who "get" v)]
    [else
     (define bstr (rktio_to_bytes v))
     (rktio_free v)
     (end-atomic)
     (bytes->string/utf-8 bstr)]))

(define/who (udp-multicast-set-interface! u hostname)
  (check who udp? u)
  (check who string? #:or-false hostname)
  (atomically
   (call-with-resolved-address
    #:who who
    #:port-number-on-error? #f
    hostname (and hostname -1)
    #:family (udp-default-family)
    #:tcp? #f
    (lambda (addr)
      (check-udp-closed who u)
      (define r (rktio_udp_set_multicast_interface rktio (udp-s u) addr))
      (when (rktio-error? r)
        (raise-option-error who "set" r))))))
  
;; ----------------------------------------

(define/who (udp-multicast-loopback? u)
  (check who udp? u)
  (atomically
   (check-udp-closed who u)
   (define v (rktio_udp_get_multicast_loopback rktio (udp-s u)))
   (cond
     [(rktio-error? v)
      (raise-option-error who "get" v)]
     [else (not (zero? v))])))

(define/who (udp-multicast-set-loopback! u loopback?)
  (check who udp? u)
  (atomically
   (check-udp-closed who u)
   (define r (rktio_udp_set_multicast_loopback rktio (udp-s u) loopback?))
   (when (rktio-error? r)
     (raise-option-error who "set" r))))
  
;; ----------------------------------------

(define/who (udp-multicast-ttl u)
  (check who udp? u)
  (atomically
   (check-udp-closed who u)
   (define v (rktio_udp_get_multicast_ttl rktio (udp-s u)))
   (cond
     [(rktio-error? v)
      (raise-option-error who "get" v)]
     [else v])))

(define/who (udp-multicast-set-ttl! u ttl)
  (check who udp? u)
  (check who byte? ttl)
  (atomically
   (check-udp-closed who u)
   (define r (rktio_udp_set_multicast_ttl rktio (udp-s u) ttl))
   (when (rktio-error? r)
     (raise-option-error who "set" r))))
