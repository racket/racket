#lang racket/base
(require "../common/resource.rkt"
         "../string/convert.rkt"
         "../host/rktio.rkt"
         "../host/thread.rkt"
         "../host/place-local.rkt"
         "evt.rkt"
         "error.rkt")

(provide call-with-resolved-address
         register-address-finalizer
         poll-address-finalizations
         address-init!)

;; in uninterruptable mode and *not* rktio mode
(define (call-with-resolved-address hostname port-no proc
                                    #:who [who #f] ; not #f => report errors
                                    #:which [which ""] ; for error reporting, including trailing space
                                    #:port-number-on-error? [port-number-on-error? #t]
                                    #:enable-break? [enable-break? #f]
                                    #:family [family RKTIO_FAMILY_ANY]
                                    #:passive? [passive? #f]
                                    #:tcp? [tcp? #t]
                                    #:retain-address? [retain-address? #f])
  (cond
    [(and (not hostname)
          (not port-no))
     (proc #f)]
    [else
     (call-with-resource
      (box (rktioly (rktio_start_addrinfo_lookup rktio
                                                 (and hostname (string->bytes/utf-8 hostname))
                                                 (or port-no 0)
                                                 family passive? tcp?)))
      ;; in uninterruptible mode (possibly atomic), *not* in rktio mode
      (lambda (lookup-box)
        (define lookup (unbox lookup-box))
        (when lookup
          (rktioly (rktio_addrinfo_lookup_stop rktio lookup))))
      ;; in uninterruptible mode, *not* rktio mode
      (lambda (lookup-box)
        (define lookup (unbox lookup-box))
        (let loop ()
          (cond
            [(and (not (rktio-error? lookup))
                  (eqv? (rktioly (rktio_poll_addrinfo_lookup_ready rktio lookup))
                        RKTIO_POLL_NOT_READY))
             (end-uninterruptible)
             ((if enable-break? sync/enable-break sync)
              (rktio-evt (lambda ()
                           (not (eqv? (rktioly (rktio_poll_addrinfo_lookup_ready rktio lookup))
                                      RKTIO_POLL_NOT_READY)))
                         ;; in atomic and in rktio-sleep-relevant, must not start nested rktio
                         (lambda (ps)
                           (rktio_poll_add_addrinfo_lookup rktio lookup ps))))
             (start-uninterruptible)
             (loop)]
            [else
             (set-box! lookup-box #f) ; receiving result implies `lookup` is destroyed
             (call-with-resource
              (if (rktio-error? lookup)
                  lookup
                  (rktioly (rktio_addrinfo_lookup_get rktio lookup)))
              ;; in uninterruptible mode (possibly atomic), *not* in rktio mode
              (lambda (addr) (rktioly (rktio_addrinfo_free rktio addr)))
              ;; in uninterruptible mode, *not* rktio mode
              (lambda (addr)
                (cond
                  [(and who (rktio-error? addr))
                   (end-uninterruptible)
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
                       (rktioly (rktio_addrinfo_free rktio addr))))])))]))))]))

;; ----------------------------------------

(define-place-local address-will-executor (make-will-executor))

(define (register-address-finalizer addr)
  (will-register address-will-executor
                 addr
                 (lambda (addr)
                   (rktioly (rktio_addrinfo_free rktio addr))
                   #t)))

;; *not* in uninterruptible mode
(define (poll-address-finalizations)
  (when (will-try-execute address-will-executor)
    (poll-address-finalizations)))

(define (address-init!)
  (set! address-will-executor (make-will-executor)))
