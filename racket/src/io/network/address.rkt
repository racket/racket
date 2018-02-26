#lang racket/base
(require "../common/resource.rkt"
         "../string/convert.rkt"
         "../host/rktio.rkt"
         "../host/thread.rkt"
         "evt.rkt"
         "error.rkt")

(provide call-with-resolved-address
         register-address-finalizer)

;; in atomic mode
(define (call-with-resolved-address hostname port-no proc
                                    #:who [who #f] ; not #f => report errors
                                    #:which [which ""] ; for error reporting, including trailing space
                                    #:port-number-on-error? [port-number-on-error? #t]
                                    #:enable-break? [enable-break? #f]
                                    #:family [family RKTIO_FAMILY_ANY]
                                    #:passive? [passive? #f]
                                    #:tcp? [tcp? #t]
                                    #:retain-address? [retain-address? #f])
  (poll-address-finalizations)
  (cond
    [(and (not hostname)
          (not port-no))
     (proc #f)]
    [else
     (call-with-resource
      (box (rktio_start_addrinfo_lookup rktio
                                        (and hostname (string->bytes/utf-8 hostname))
                                        (or port-no 0)
                                        family passive? tcp?))
      ;; in atomic mode
      (lambda (lookup-box)
        (define lookup (unbox lookup-box))
        (when lookup
          (rktio_addrinfo_lookup_stop lookup)))
      ;; in atomic mode
      (lambda (lookup-box)
        (define lookup (unbox lookup-box))
        (let loop ()
          (cond
            [(and (not (rktio-error? lookup))
                  (eqv? (rktio_poll_addrinfo_lookup_ready rktio lookup)
                        RKTIO_POLL_NOT_READY))
             (end-atomic)
             ((if enable-break? sync/enable-break sync)
              (rktio-evt (lambda ()
                           (not (eqv? (rktio_poll_addrinfo_lookup_ready rktio lookup)
                                      RKTIO_POLL_NOT_READY)))
                         (lambda (ps)
                           (rktio_poll_add_addrinfo_lookup rktio lookup ps))))
             (start-atomic)
             (loop)]
            [else
             (set-box! lookup-box #f) ; receiving result implies `lookup` is destroyed
             (call-with-resource
              (if (rktio-error? lookup)
                  lookup
                  (rktio_addrinfo_lookup_get rktio lookup))
              ;; in atomic mode
              (lambda (addr) (rktio_addrinfo_free rktio addr))
              ;; in atomic mode
              (lambda (addr)
                (cond
                  [(and who (rktio-error? addr))
                   (raise-network-error who addr (string-append
                                                  "can't resolve " which "address"
                                                  "\n  address: " (or hostname "<unspec>")
                                                  (if (and port-number-on-error? port-no)
                                                      (string-append "\n  port number: " (number->string port-no))
                                                      "")))]
                  [else
                   ;; `addr` may be an error; if so, let `proc` handle it
                   (begin0
                     (proc addr)
                     (unless retain-address?
                       (rktio_addrinfo_free rktio addr)))])))]))))]))

;; ----------------------------------------

(define address-will-executor (make-will-executor))

(define (register-address-finalizer addr)
  (will-register address-will-executor
                 addr
                 (lambda (addr)
                   (rktio_addrinfo_free rktio addr)
                   #t)))

(define (poll-address-finalizations)
  (when (will-try-execute address-will-executor)
    (poll-address-finalizations)))
