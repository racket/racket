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

(define netcat
  (for/first ([netcat '("/bin/nc" "/usr/bin/nc")]
              #:when (and (file-exists? netcat)
                          (memq 'execute (file-or-directory-permissions netcat))))
    netcat))

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

       (define-check (check-comm msg out in)
         (write-bytes msg out)
         (flush-output out)
         (check-equal? (sync/timeout 1 (read-bytes-evt (bytes-length msg) in))
                       msg))

       (check-comm #"hello" to-sock ncout)
       (check-comm #"charmed" ncin from-sock)
       (check-comm #"well\ngoodbye, then" to-sock ncout)

       (close-output-port to-sock)
       (close-input-port from-sock)

       (close-output-port ncin)
       (close-input-port ncout)
       (close-input-port ncerr)
       (or (sync/timeout 1 ncprocess)
           (subprocess-kill ncprocess))
       ))
    (when (file-exists? tmp) (delete-file tmp)))]
 [else
  (eprintf "cannot test unix sockets\n")
  (unless unix-socket-available?
    (eprintf "unix sockets not supported\n"))
  (unless netcat
    (eprintf "netcat not available\n"))])
