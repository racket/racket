#lang racket/base

(module+ test
  (require net/server
           racket/list
           racket/port
           racket/promise
           racket/tcp
           rackunit)

  (define ((make-tcp-listen-proc port-ch) port backlog reuse? host)
    (define listener
      (tcp-listen port backlog reuse? host))
    (define-values (_local-host local-port _remote-host _remote-port)
      (tcp-addresses listener #t))
    (thread (λ () (channel-put port-ch local-port)))
    listener)

  (define (echo in out)
    (define buf (make-bytes 4096))
    (let loop ()
      (define n-read
        (read-bytes-avail! buf in))
      (unless (eof-object? n-read)
        (write-bytes buf out 0 n-read)
        (flush-output out)
        (loop))))

  (define (client port handle)
    (define-values (in out)
      (tcp-connect "127.0.0.1" port))
    (dynamic-wind
      void
      (lambda ()
        (let loop ()
          (handle in out loop)))
      (lambda ()
        (close-output-port out)
        (close-input-port in))))

  (define start-test-server
    (make-keyword-procedure
     (lambda (kws kw-args [handler-proc echo] . args)
       (define port-ch (make-channel))
       (define stop
         (keyword-apply
          start-server
          kws kw-args
          "127.0.0.1" 0 handler-proc
          args
          #:listen-proc (make-tcp-listen-proc port-ch)))
       (values stop (channel-get port-ch)))))

  (define start-test-server*
    (make-keyword-procedure
     (lambda (kws kw-args . args)
       (define cust (make-custodian))
       (define-values (stop port)
         (parameterize ([current-custodian cust])
           (keyword-apply start-test-server kws kw-args args)))
       (define (stop*)
         (stop)
         (custodian-shutdown-all cust))
       (values stop* port))))

  (define (call-with-stop stop-proc proc)
    (define cust (make-custodian))
    (parameterize ([current-custodian cust])
      (dynamic-wind
        void
        (lambda ()
          (proc))
        (lambda ()
          (stop-proc)
          (custodian-shutdown-all cust)))))

  (test-case "echo server"
    (define-values (stop port)
      (start-test-server*))
    (call-with-stop stop
      (lambda ()
        (define n 100)
        (define res
          (for/list/concurrent ([_ (in-range n)])
            (client port (λ (in out _k)
                           (write-bytes #"hello" out)
                           (close-output-port out)
                           (port->bytes in)))))
        (check-equal? res (make-list n #"hello"))))
    (check-exn
     #rx"Connection refused"
     (lambda ()
       (tcp-connect "127.0.0.1" port))))

  (test-case "failing echo server handler"
    (define err-out (open-output-string))
    (define-values (stop port)
      (parameterize ([current-error-port err-out])
        (start-test-server*
         (λ (in out)
           (if (>= (random) 0.5)
               (echo in out)
               (error 'fail))))))
    (call-with-stop stop
      (lambda ()
        (define n 100)
        (define res
          (for/list/concurrent ([_ (in-range n)])
            (client port (λ (in out _k)
                           (write-bytes #"hello" out)
                           (close-output-port out)
                           (port->bytes in)))))
        (define grouped-res
          (sort (group-by values res) #:key car bytes<?))
        (check-true (= 2 (length grouped-res)))
        (check-equal? (caar grouped-res) #"")
        (check-equal? (caadr grouped-res) #"hello")
        (check-true
         (regexp-match?
          #rx"error: fail"
          (get-output-string err-out))))))

  (test-case "echo server with timeout"
    (define err-out (open-output-string))
    (define-values (stop port)
      (parameterize ([current-error-port err-out])
        (start-test-server*
         #:timeout-evt-proc
         (λ (_thd _in _out _break-sent?)
           (alarm-evt (+ (current-inexact-monotonic-milliseconds) 100) #t)))))
    (call-with-stop stop
      (lambda ()
        (define start-time (current-inexact-monotonic-milliseconds))
        (define client-thd
          (thread
           (lambda ()
             (with-handlers ([exn:fail:network? void])
               (client port (λ (_in out k)
                              (write-bytes #"hello" out)
                              (flush-output out)
                              (sleep 0.05)
                              (k)))))))
        (sync/timeout 1.0 client-thd)
        (check-true (< (- (current-inexact-monotonic-milliseconds) start-time) 1000))
        (check-true (regexp-match? #rx"user break" (get-output-string err-out))))))

  (test-case "echo server with limited concurrency"
    (define connected-sema (make-semaphore))
    (define-values (stop port)
      (start-test-server*
       #:max-concurrent 1
       (λ (in out)
         (semaphore-post connected-sema)
         (echo in out))))
    (define (run-client close-sema)
      (client port (λ (_in out _k)
                     (write-bytes #"hello" out)
                     (flush-output out)
                     (semaphore-wait close-sema))))
    (call-with-stop stop
      (lambda ()
        (define close-sema (make-semaphore))
        (define client-1 (thread (λ () (run-client close-sema))))
        (sync (system-idle-evt))
        (define client-2 (thread (λ () (run-client close-sema))))
        (check-not-false (sync/timeout 1.0 connected-sema))
        (check-false (sync/timeout 0 connected-sema))
        (semaphore-post close-sema)
        (thread-wait client-1)
        (check-not-false (sync/timeout 1.0 connected-sema))
        (check-false (sync/timeout 0 connected-sema))
        (semaphore-post close-sema)
        (thread-wait client-2))))

  (test-case "connections can outlive servers"
    (define-values (stop port)
      (start-test-server))
    (define-values (in out)
      (tcp-connect "127.0.0.1" port))
    (dynamic-wind
      void
      (lambda ()
        ;; Give the server time to accept the connection before
        ;; sending a break.
        (sync (system-idle-evt))
        (stop)
        (write-bytes #"hello" out)
        (flush-output out)
        (check-equal? (read-bytes 5 in) #"hello"))
      (lambda ()
        (close-output-port out)
        (close-input-port in))))

  (test-case "breaks in connection handlers"
    (for ([on? (in-list '(#f #t))])
      (define break-enabled?-ch
        (make-channel))
      (define-values (stop port)
        (parameterize-break on?
          (start-test-server
           (λ (_in _out)
             (channel-put break-enabled?-ch (break-enabled))))))
      (call-with-stop stop
        (lambda ()
          (define-values (in out)
            (tcp-connect "127.0.0.1" port))
          (close-output-port out)
          (close-input-port in)
          (check-eq? (channel-get break-enabled?-ch) on?)))))

  (test-case "run-server"
    (define port-ch (make-channel))
    (define server-thd
      (thread
       (lambda ()
         (run-server
          #:listen-proc (make-tcp-listen-proc port-ch)
          "127.0.0.1" 0 echo))))
    (define (stop)
      (break-thread server-thd)
      (thread-wait server-thd))
    (define port (sync/timeout 1.0 port-ch))
    (call-with-stop stop
      (lambda ()
        (check-equal?
         (client port (λ (in out _k)
                        (write-bytes #"hello" out)
                        (close-output-port out)
                        (read-bytes 5 in)))
         #"hello")))
    (check-exn
     #rx"Connection refused"
     (lambda ()
       (tcp-connect "127.0.0.1" port)))))

;; Local Variables:
;; eval: (put 'call-with-stop 'racket-indent-function #'defun)
;; End:
