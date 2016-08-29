#lang racket/base
; It may look like an HTTPS server, but it very isn’t
(provide server
         current-conn-timeout)

(require racket/match
         openssl
         syntax/modresolve
         "generic-server.rkt")

(define (conn-proc i o)  
  (define ssl-srvr-ctx (ssl-make-server-context 'secure))
  (define test.pem-path (build-path
                         (let-values (([base name mbd?]
                                       (split-path (resolve-module-path 'openssl)))) base)
                         "test.pem"))
  (ssl-load-certificate-chain! ssl-srvr-ctx test.pem-path)
  (ssl-load-private-key! ssl-srvr-ctx test.pem-path)
  (define-values (s:i s:o)
    (ports->ssl-ports i o
                      #:mode 'accept
                      #:context ssl-srvr-ctx
                      #:close-original? #t
                      #:shutdown-on-close? #t))
  (define request-lines
    (for/list ((l (in-lines s:i 'return-linefeed)) #:break (string=? l "")) l))
  (define-syntax-rule (out/flush fmt args ...)
    (begin (fprintf s:o fmt args ...) (flush-output s:o)))
  
  (match request-lines
    [(cons (regexp #px"^(GET)\\s+(\\S+)(\\s+HTTP/\\S+)?$" (list _ method uri _)) _)
     (define content (format "~s (but at least it's secure)" uri))
     (out/flush
      "HTTP/1.1 200 OK\r\nContent-type: text/html\r\nContent-length: ~a\r\n\r\n~a"
      (string-length content) content)]
    [(cons (regexp #px"^(\\S+)\\s+(\\S+)(\\s+HTTP/\\S+)?$"
                   (list request method request-uri http-version)) _)
     (out/flush "HTTP/1.1 405 Method Not Allowed\r\n\r\n")]
    [_ (out/flush "HTTP/1.1 400 Bad Request\r\n\r\n")]))

(define (server) (serve conn-proc))

(module+
  main
  (define-values (the-port server-thread shutdown-server) (server))
  (dynamic-wind void (λ () (thread-wait server-thread)) shutdown-server))

(module+ test)
