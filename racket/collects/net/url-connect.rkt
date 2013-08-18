#lang racket/base

(require (rename-in racket/tcp
                    [tcp-connect plain-tcp-connect]
                    [tcp-abandon-port plain-tcp-abandon-port])
         openssl
         "win32-ssl.rkt")

(provide (all-defined-out))

(define current-connect-scheme (make-parameter "http"))

(define current-https-protocol (make-parameter 'sslv2-or-v3))

;; Define `tcp-connect' and `tcp-abandon-port' to fit with
;; `current-connect-scheme'
(define (tcp-connect host port)
  (cond [(equal? (current-connect-scheme) "https")
         (if (or ssl-available?
                 (not win32-ssl-available?))
             (ssl-connect host port (current-https-protocol))
             (win32-ssl-connect host port (current-https-protocol)))]
        [else
         (plain-tcp-connect host port)]))

(define (tcp-abandon-port port)
  (cond [(ssl-port? port) (ssl-abandon-port port)]
        [(win32-ssl-port? port) (win32-ssl-abandon-port port)]
        [else (plain-tcp-abandon-port port)]))
