#lang racket
(require racket/port
         rackunit
         unstable/socket)

(define (call-in-custodian proc)
  (parameterize ((current-subprocess-custodian-mode 'kill))
    (parameterize ((current-custodian (make-custodian)))
      (call-with-continuation-barrier
       (lambda ()
         (dynamic-wind void
                       proc
                       (lambda ()
                         (custodian-shutdown-all (current-custodian)))))))))

;; Commands for creating socket listeners
;;  - netcat is commonly available, but cannot use Linux abstract namespace
;;  - socat can use Linux abstract namespace, but is less common
;; So use netcat for path test and socat for abstract-name test.

(define netcat
  (for/first ([netcat '("/bin/nc" "/usr/bin/nc")]
              #:when (and (file-exists? netcat)
                          (memq 'execute (file-or-directory-permissions netcat))))
    netcat))

(define socat
  (for/first ([socat '("/usr/bin/socat")]
              #:when (and (file-exists? socat)
                          (memq 'execute (file-or-directory-permissions socat))))
    socat))

(define-check (check-comm msg out in)
  (write-bytes msg out)
  (flush-output out)
  (check-equal? (sync/timeout 1 (read-bytes-evt (bytes-length msg) in))
                msg))

(define (close-ports . ports)
  (for ([port ports])
    (cond [(input-port? port) (close-input-port port)]
          [(output-port? port) (close-output-port port)])))

;; Test path-based socket

(cond
 [(and unix-socket-available? netcat)
  (test-case "unix socket"
    ;; Uses netcat to create a simple unix domain socket server
    (define tmp ((values make-temporary-file)))
    (delete-file tmp)
    (call-in-custodian
     (lambda ()
       (define-values (ncprocess ncout ncin ncerr)
         (subprocess #f #f #f netcat "-Ul" (path->string tmp)))
       (sleep 0.5)
       (define-values (from-sock to-sock)
         (unix-socket-connect tmp))
       (check-comm #"hello" to-sock ncout)
       (check-comm #"charmed" ncin from-sock)
       (check-comm #"well\ngoodbye, then" to-sock ncout)
       (close-ports to-sock from-sock)
       (close-ports ncin ncout ncerr)
       (or (sync/timeout 1 ncprocess)
           (subprocess-kill ncprocess))
       ))
    (when (file-exists? tmp) (delete-file tmp)))]
 [else
  (printf "cannot test unix sockets: ~a\n"
          (if netcat
              "unix sockets not supported"
              "netcat not found"))])

;; Test Linux abstract name socket

(cond
 [(and unix-socket-available? socat)
  (test-case "unix socket w/ abstract name"
    ;; Uses socat to create a simple unix domain socket server
    (call-in-custodian
     (lambda ()
       (define name #"TestRacketABC")
       (define-values (ncprocess ncout ncin ncerr)
         (subprocess #f #f #f socat (format "ABSTRACT-LISTEN:~a" name) "STDIO"))
       (sleep 0.5)
       (define-values (from-sock to-sock)
         (unix-socket-connect (bytes-append #"\0" name)))
       (check-comm #"hello" to-sock ncout)
       (check-comm #"charmed" ncin from-sock)
       (check-comm #"well\ngoodbye, then" to-sock ncout)
       (close-ports to-sock from-sock)
       (close-ports ncin ncout ncerr)
       (or (sync/timeout 1 ncprocess)
           (subprocess-kill ncprocess))
       (void)
       )))]
 [(not socat)
  (printf "cannot test unix sockets w/ abstract namespace: socat not found\n")])
