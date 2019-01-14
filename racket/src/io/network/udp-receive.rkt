#lang racket/base
(require "../host/place-local.rkt"
         "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../sandman/main.rkt"
         "../string/convert.rkt"
         "../string/integer.rkt"
         "../format/main.rkt"
         "port-number.rkt"
         "check.rkt"
         "address.rkt"
         "udp-socket.rkt"
         "error.rkt"
         "evt.rkt")

(provide udp-receive!
         udp-receive!*
         udp-receive!/enable-break

         udp-receive!-evt
         udp-receive-ready-evt

         udp-set-receive-buffer-size!)

(define/who (udp-receive! u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (do-udp-receive! who u bstr start end))

(define/who (udp-receive!* u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (do-udp-receive! who #:wait? #f u bstr start end))

(define/who (udp-receive!/enable-break u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (do-udp-receive! who #:enable-break? #t u bstr start end))

(define (do-udp-receive! who u bstr start end
                         #:wait? [wait? #t]
                         #:enable-break? [enable-break? #f])
  (check-receive! who u bstr start end)
  (atomically
   (do-udp-maybe-receive! who u bstr start end
                          #:wait? wait?
                          #:enable-break? enable-break?)))

(define/who (udp-receive!-evt u bstr [start 0] [end (and (bytes? bstr) (bytes-length bstr))])
  (check-receive! who u bstr start end)
  (udp-receiving-evt
   u
   ;; in atomic mode:
   (lambda ()
     (do-udp-maybe-receive! who u bstr start end
                            #:wait? #f
                            #:handle-error (lambda (thunk) thunk)))))
  
(define/who (udp-receive-ready-evt u)
  (check who udp? u)
  (udp-receiving-ready-evt
   (lambda ()
     (or (not (udp-s u))
         (not (eqv? (rktio_poll_read_ready rktio (udp-s u))
                    RKTIO_POLL_NOT_READY))))
   (lambda (ps)
     (rktio_poll_add rktio (udp-s u) ps RKTIO_POLL_READ))))

(define (check-receive! who u bstr start end)
  (check who udp? u)
  (check-bstr who bstr start end))

;; ----------------------------------------

;; in atomic mode
(define (do-udp-maybe-receive! who u bstr start end
                               #:wait? [wait? #t]
                               #:enable-break? [enable-break? #f]
                               #:handle-error [handle-error handle-error-immediately])
   (let loop ()
     ;; re-check closed on every iteration, in case the state changes
     ;; while we block
     (check-udp-closed
      who u
      #:handle-error handle-error
      #:continue
      (lambda ()
        (cond
          [(not (udp-bound? u))
           (handle-error
            (lambda ()
              (raise-network-arguments-error who "udp socket is not bound"
                                             "socket" u)))]
          [else
           (define r (rktio_udp_recvfrom_in rktio (udp-s u) bstr start end))
           (cond
             [(rktio-error? r)
              (cond
                [(or (racket-error? r RKTIO_ERROR_TRY_AGAIN)
                     (racket-error? r RKTIO_ERROR_INFO_TRY_AGAIN))
                 (cond
                   [wait?
                    (end-atomic)
                    ((if enable-break? sync/enable-break sync)
                     (rktio-evt (lambda ()
                                  (or (not (udp-s u))
                                      (not (eqv? (rktio_poll_read_ready rktio (udp-s u))
                                                 RKTIO_POLL_NOT_READY))))
                                (lambda (ps)
                                  (rktio_poll_add rktio (udp-s u) ps RKTIO_POLL_READ))))
                    (start-atomic)
                    (loop)]
                   [else (values #f #f #f)])]
                [else
                 (handle-error
                  (lambda ()
                    (raise-network-error who r "receive failed")))])]
             [else
              (define len (rktio_recv_length_ref r))
              (define address (rktio_to_bytes_list (rktio_recv_address_ref r) 2))
              (rktio_free r)
              (values len
                      (if (bytes=? (car address) cached-address-bytes)
                          cached-address-string
                          (begin
                            (set! cached-address-bytes (car address))
                            (set! cached-address-string (string->immutable-string
                                                         (bytes->string/utf-8 cached-address-bytes #\?)))
                            cached-address-string))
                      (string->integer (bytes->string/utf-8 (cadr address))))])])))))

(define-place-local cached-address-bytes #"")
(define-place-local cached-address-string "")

;; ----------------------------------------

(struct udp-receiving-evt (u try)
  #:property
  prop:evt
  (poller
   ;; in atomic mode
   (lambda (self poll-ctx)
     (define try (udp-receiving-evt-try self))
     (call-with-values try
       (case-lambda
         [(thunk)
          ;; `thunk` that raises an exception
          (values #f (wrap-evt always-evt (lambda (v) (thunk))))]
         [(r hostname port-no)
          (cond
            [r
             (values (list (list r hostname port-no)) #f)]
            [else
             (sandman-poll-ctx-add-poll-set-adder!
              poll-ctx
              (lambda (ps)
                (rktio_poll_add rktio (udp-s (udp-receiving-evt-u self)) ps RKTIO_POLL_WRITE)))
             (values #f self)])]))))
  #:reflection-name 'udp-receive-evt
  #:authentic)

(struct udp-receiving-ready-evt rktio-evt ()
  #:reflection-name 'udp-receive-ready-evt
  #:authentic)

;; ----------------------------------------

(define/who (udp-set-receive-buffer-size! u size)
  (check who udp? u)
  (check who exact-positive-integer? size)
  (atomically
   (check-udp-closed who u)
   (unless (fixnum? size)
     (end-atomic)
     (raise-non-fixnum who size))
   (define r (rktio_udp_set_receive_buffer_size rktio (udp-s u) size))
   (when (rktio-error? r)
     (raise-option-error who "set" r))))

(define (raise-option-error who mode v)
  (end-atomic)
  (raise-network-error who v (string-append mode "sockopt failed")))

(define (raise-non-fixnum who size)
  (raise (exn:fail:network
          (format (string-append "~a: given size is too large\n"
                                 "  given size: ~e")
                  who
                  size)
          (current-continuation-marks))))
