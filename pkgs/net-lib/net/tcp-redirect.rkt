#lang racket/base
(provide tcp-redirect)

(require racket/unit racket/tcp racket/async-channel "tcp-sig.rkt")

(define raw:tcp-abandon-port tcp-abandon-port)
(define raw:tcp-accept tcp-accept)
(define raw:tcp-accept/enable-break tcp-accept/enable-break)
(define raw:tcp-accept-ready? tcp-accept-ready?)
(define raw:tcp-addresses tcp-addresses)
(define raw:tcp-close tcp-close)
(define raw:tcp-connect tcp-connect)
(define raw:tcp-connect/enable-break tcp-connect/enable-break)
(define raw:tcp-listen tcp-listen)
(define raw:tcp-listener? tcp-listener?)

;; For tcp-listeners, we use an else branch in the conds since
;; (instead of a contract) I want the same error message as the raw
;; primitive for bad inputs.

;; : (listof nat) -> (unit/sig () -> net:tcp^)
(define (tcp-redirect redirected-ports [redirected-address "127.0.0.1"])
  (unit
   (import)
   (export tcp^)
   ;; : (make-pipe-listener nat (channel (cons iport oport)))
   (define-struct pipe-listener (port channel))

   ;; : port -> void
   (define (tcp-abandon-port tcp-port)
     (when (tcp-port? tcp-port)
       (raw:tcp-abandon-port tcp-port)))

   ;; : listener -> iport oport
   (define (tcp-accept tcp-listener)
     (cond
       [(pipe-listener? tcp-listener)
        (let ([in-out (async-channel-get (pipe-listener-channel tcp-listener))])
          (values (car in-out) (cdr in-out)))]
       [else (raw:tcp-accept tcp-listener)]))

   ;; : listener -> iport oport
   (define (tcp-accept/enable-break tcp-listener)
     (cond
       [(pipe-listener? tcp-listener)
        ;; XXX put this into async-channel.rkt as async-channel-get/enable-break
        (sync/enable-break
         (handle-evt
          (pipe-listener-channel tcp-listener)
          (lambda (in-out)
            (values (car in-out) (cdr in-out)))))]
       #;(let ([in-out (async-channel-get (pipe-listener-channel tcp-listener))])
           (values (car in-out) (cdr in-out)))
       [else (raw:tcp-accept/enable-break tcp-listener)]))

   ;; : tcp-listener -> iport oport
   ;; FIX - check channel queue size
   (define (tcp-accept-ready? tcp-listener)
     (cond
       [(pipe-listener? tcp-listener) #t]
       [else (raw:tcp-accept-ready? tcp-listener)]))

   ;; : tcp-port -> str str
   (define (tcp-addresses tcp-port)
     (if (tcp-port? tcp-port)
       (raw:tcp-addresses tcp-port)
       (values redirected-address redirected-address)))

   ;; : port -> void
   (define (tcp-close tcp-listener)
     (if (tcp-listener? tcp-listener)
       (raw:tcp-close tcp-listener)
       (hash-remove! port-table (pipe-listener-port tcp-listener))))

   ;; : (str nat -> iport oport) -> str nat -> iport oport
   (define (gen-tcp-connect raw)
     (lambda (hostname-string port)
       (if (and (string=? redirected-address hostname-string)
                (redirect? port))
         (let-values ([(to-in from-out) (make-pipe)]
                      [(from-in to-out) (make-pipe)])
           (async-channel-put
            (pipe-listener-channel
             (hash-ref port-table port
                       (lambda ()
                         (raise (make-exn:fail:network
                                 (format "tcp-connect: connection to ~a, port ~a failed (nobody is listening)"
                                         hostname-string port)
                                 (current-continuation-marks))))))
            (cons to-in to-out))
           (values from-in from-out))
         (raw hostname-string port))))

   ;; : str nat -> iport oport
   (define tcp-connect (gen-tcp-connect raw:tcp-connect))

   ;; : str nat -> iport oport
   (define tcp-connect/enable-break
     (gen-tcp-connect raw:tcp-connect/enable-break))

   ;; FIX - support the reuse? flag.
   (define (tcp-listen port [max-allow-wait 4] [reuse? #f] [hostname-string #f])
     (hash-ref port-table port
       (lambda ()
         (if (redirect? port)
           (let ([listener (make-pipe-listener port (make-async-channel))])
             (hash-set! port-table port listener)
             listener)
           (raw:tcp-listen port max-allow-wait reuse? hostname-string)))))

   ;; : tst -> bool
   (define (tcp-listener? x)
     (or (pipe-listener? x) (raw:tcp-listener? x)))

   ;; ---------- private ----------

   ;; : (hash nat[port] -> tcp-listener)
   (define port-table (make-hasheq))

   (define redirect-table
     (let ([table (make-hasheq)])
       (for-each (lambda (x) (hash-set! table x #t))
                 redirected-ports)
       table))

   ;; : nat -> bool
   (define (redirect? port)
     (hash-ref redirect-table port #f))

   ))
