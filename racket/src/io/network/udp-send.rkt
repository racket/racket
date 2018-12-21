#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../sandman/main.rkt"
         "../security/main.rkt"
         "port-number.rkt"
         "check.rkt"
         "address.rkt"
         "udp-socket.rkt"
         "error.rkt"
         "evt.rkt")

(provide udp-send
         udp-send*
         udp-send-to/enable-break
         
         udp-send-to
         udp-send-to*
         udp-send/enable-break
         
         udp-send-evt
         udp-send-to-evt
         udp-send-ready-evt)

(define/who (udp-send u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send who u bstr start end)
  (do-udp-send-to who u #f #f bstr start end))

(define/who (udp-send* u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send who u bstr start end)
  (do-udp-send-to who #:wait? #f u #f #f bstr start end))

(define/who (udp-send/enable-break u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send who u bstr start end)
  (do-udp-send-to who #:enable-break? #t u #f #f bstr start end))

(define/who (udp-send-to u hostname port-no bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send-to who u hostname port-no bstr start end)
  (do-udp-send-to who u hostname port-no bstr start end))

(define/who (udp-send-to* u hostname port-no bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send-to who u hostname port-no bstr start end)
  (do-udp-send-to who #:wait? #f u hostname port-no bstr start end))

(define/who (udp-send-to/enable-break u hostname port-no bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send-to who u hostname port-no bstr start end)
  (do-udp-send-to who #:enable-break? #t u hostname port-no bstr start end))

(define/who (udp-send-evt u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send who u bstr start end)
  (do-udp-send-to-evt who u #f #f bstr start end))

(define/who (udp-send-to-evt u hostname port-no bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-send-to who u hostname port-no bstr start end)
  (do-udp-send-to-evt who u hostname port-no bstr start end))

(define/who (udp-send-ready-evt u)
  (check who udp? u)
  (udp-sending-ready-evt
   (lambda ()
     (or (not (udp-s u))
         (not (eqv? (rktio_poll_write_ready rktio (udp-s u))
                    RKTIO_POLL_NOT_READY))))
   (lambda (ps)
     (rktio_poll_add rktio (udp-s u) ps RKTIO_POLL_WRITE))))

;; ----------------------------------------

(define (check-send who u bstr start end)
  (check who udp? u)
  (check-bstr who bstr start end))

(define (check-send-to who u hostname port-no bstr start end)
  (check who udp? u)
  (check who string? hostname)
  (check who port-number? port-no)
  (check-bstr who bstr start end)
  (security-guard-check-network who hostname port-no 'client))

;; ----------------------------------------

(define (do-udp-send-to who u hostname port-no bstr start end
                        #:wait? [wait? #t]
                        #:enable-break? [enable-break? #f])
  (atomically
   (call-with-resolved-address
    #:who who
    hostname port-no
    #:tcp? #f
    (lambda (addr)
      (do-udp-maybe-send-to-addr who u addr bstr start end
                                 #:wait? wait?
                                 #:enable-break? enable-break?)))))

(define (do-udp-send-to-evt who u hostname port-no bstr start end)
  (atomically
   (call-with-resolved-address
    #:who who
    hostname port-no
    #:tcp? #f
    #:retain-address? #t
    (lambda (addr)
      ;; FIXME: need to finalize `addr`
      (udp-sending-evt
       u
       ;; in atomic mode:
       (lambda ()
         (when addr (register-address-finalizer addr))
         (do-udp-maybe-send-to-addr who u addr bstr start end
                                    #:wait? #f
                                    #:handle-error (lambda (thunk) thunk))))))))

; in atomic mode
(define (do-udp-maybe-send-to-addr who u addr bstr start end
                                   #:wait? [wait? #t]
                                   #:enable-break? [enable-break? #f]
                                   #:handle-error [handle-error handle-error-immediately])
  (let loop ()
    ;; re-check closed, connected, etc., on every iteration,
    ;; in case the state changes while we block
    (check-udp-closed
     who u
     #:handle-error handle-error
     #:continue
     (lambda ()
       (cond
         [(and addr (udp-connected? u))
          (handle-error
           (lambda ()
             (raise-network-arguments-error who "udp socket is connected"
                                            "socket" u)))]
         [(and (not addr) (not (udp-connected? u)))
          (handle-error
           (lambda ()
             (raise-network-arguments-error who "udp socket is not connected"
                                            "socket" u)))]
         [else
          ;; if the socket is not bound already, send[to] binds it
          (set-udp-is-bound?! u #t)
          (define r (rktio_udp_sendto_in rktio (udp-s u) addr bstr start end))
          (cond
            [(rktio-error? r)
             (handle-error
              (lambda ()
                (raise-network-error who r "send failed")))]
            [(eqv? r 0)
             (cond
               [(not wait?) #f]
               [else
                (end-atomic)
                ((if enable-break? sync/enable-break sync)
                 (rktio-evt (lambda ()
                              (or (not (udp-s u))
                                  (not (eqv? (rktio_poll_write_ready rktio (udp-s u))
                                             RKTIO_POLL_NOT_READY))))
                            (lambda (ps)
                              (rktio_poll_add rktio (udp-s u) ps RKTIO_POLL_WRITE))))
                (start-atomic)
                (loop)])]
            [(= r (- end start)) (if wait? (void) #t)]
            [else
             (handle-error
              (lambda ()
                (raise
                 (exn:fail:network
                  (string-append (symbol->string who) ": didn't send enough"
                                 "\n  requested bytes: " (number->string (- end start))
                                 "\n  sent bytes: " r)
                  (current-continuation-marks)))))])])))))

;; ----------------------------------------

(struct udp-sending-evt (u try)
  #:property
  prop:evt
  (poller
   ;; in atomic mode
   (lambda (self poll-ctx)
     (define try (udp-sending-evt-try self))
     (define r (try))
     (cond
       [(procedure? r)
        ;; `r` is a thunk that raises an exception
        (values #f (wrap-evt always-evt (lambda (v) (r))))]
       [r
        (values (list (void)) #f)]
       [else
        (sandman-poll-ctx-add-poll-set-adder!
         poll-ctx
         (lambda (ps)
           (rktio_poll_add rktio (udp-s (udp-sending-evt-u self)) ps RKTIO_POLL_READ)))
        (values #f self)])))
  #:reflection-name 'udp-send-evt
  #:authentic)

(struct udp-sending-ready-evt rktio-evt ()
  #:reflection-name 'udp-send-ready-evt
  #:authentic)
