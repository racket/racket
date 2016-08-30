#lang racket/base
;; A proxy HTTP server -- don’t get your hopes up it’s for testing and
;; only proxies ports, probably oozes security leaks and I wouldn’t be
;; surprised if it leaked fids too.
(require racket/port racket/match racket/tcp "generic-server.rkt")

(provide server
         current-conn-timeout)

(define (http-tunnel-serve in out)
  (let/ec
    ec
    (define-syntax-rule (out/flush fmt args ...)
                        (begin (fprintf out fmt args ...)
                               (flush-output out)))

    (define request-lines (for/list ((l (in-lines in 'return-linefeed))
                                     #:break (string=? l ""))
                                    l))

    ;; frankly, I don’t care about the headers... it’s just the request string
    ;; I’m interested in
    (match request-lines
           [(cons (regexp #px"^(CONNECT)\\s+(\\S+):(\\d+)(\\s+HTTP/\\S+)?$"
                          (list _ method connect-host (app string->number connect-port) _)) _)
            (define-values (connect:from connect:to)
              (with-handlers ([exn:fail? (lambda (x)
                                           ;; any better ideas as to a good status code?
                                           (out/flush "HTTP/1.1 410 Gone\r\n\r\n")
                                           (ec))])
                             (tcp-connect connect-host connect-port)))
            (file-stream-buffer-mode connect:to 'none)
            (file-stream-buffer-mode connect:from 'none)
            (out/flush "HTTP/1.1 200 Connection Established\r\n\r\n")
            (define copy-in-to-connect:to-thread
              (thread (lambda ()
                        (copy-port in connect:to)
                        (close-output-port connect:to))))
            (define copy-connect:from-to-out-thread
              (thread (lambda ()
                        (copy-port connect:from out)
                        (close-output-port out))))
            (thread-wait copy-in-to-connect:to-thread)
            (thread-wait copy-connect:from-to-out-thread)]
           [(cons (regexp #px"^(\\S+)\\s+(\\S+)(\\s+HTTP/\\S+)?$"
                          (list request method request-uri http-version)) _)
            (out/flush "HTTP/1.1 405 Method Not Allowed\r\n\r\n")]
           [_ (out/flush "HTTP/1.1 400 Bad Request\r\n\r\n")])))

(define (server)
  (serve http-tunnel-serve))

(module+
  main
  (define-values (the-port server-thread shutdown-server)
    (server))
  (thread-wait server-thread))

(module+
  test
  (module config info
    (define timeout 300))
  (require rackunit)

  (require (prefix-in es: "echo-server.rkt"))

  (define-values (proxy-listen-port server-thread shutdown-server)
    (server))

  (define-values (echo-port es:server-thread es:shutdown-server) (es:server))
  
  (let ((old-exit-handler (exit-handler)))
    (exit-handler (lambda (exit-code)
                    (shutdown-server)
                    (es:shutdown-server)
                    (old-exit-handler exit-code))))

  (define (connect/test method uri http-version
                        #:headers (headers '())
                        #:body (body #f))

    (define-values (cl:from cl:to) (tcp-connect "localhost" proxy-listen-port))
    (file-stream-buffer-mode cl:from 'none)
    (file-stream-buffer-mode cl:to 'none)

    (if http-version
      (fprintf cl:to "~a ~a ~a\r\n" method uri http-version)
      (fprintf cl:to "~a ~a\r\n" method uri))

    (for-each (lambda (h) (fprintf cl:to "~a\r\n" h)) headers)
    (fprintf cl:to "\r\n") ; end headers

    ;; Not interested in any fancy interaction here... just see what the response is
    (when body (write-string body cl:to))
    (flush-output cl:to)
    (close-output-port cl:to)
    (begin0
      (port->string cl:from)
      (tcp-abandon-port cl:to)
      (tcp-abandon-port cl:from)))

  (check-match (connect/test "GET" "/" #f) (regexp #px"^HTTP/\\S+\\s+405"))
  (check-match (connect/test "A B" "/" #f) (regexp #px"^HTTP/\\S+\\s+400"))
  (check-match (connect/test "CONNECT" "q.com:9887" #f) (regexp #px"^HTTP/\\S+\\s+410"))
  (check-match (connect/test "CONNECT" (format "localhost:~a" echo-port)
                             #f #:body "blah blah blah!")
               (regexp #px"^HTTP/\\S+\\s+200.*blah!$"))

  )
