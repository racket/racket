#lang racket
(require tests/eli-tester
         web-server/private/timer
         web-server/private/connection-manager
         web-server/http/response
         web-server/http
         "../util.rkt")

(define (write-response r [redact? #t])
  (define-values (i-port o-port) (make-pipe))
  (define conn
    (connection 0 (start-timer +inf.0 void)
                i-port o-port (current-custodian) #t))
  (output-response conn r)
  (close-output-port o-port)
  (define bs (port->bytes i-port))
  (if redact? (redact bs) bs))

(test
 (write-response (response/xexpr '(a ([href "#"]) "link")))
 =>
 #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<a href=\"#\">link</a>"
 
 (write-response (response/xexpr '(a ([href "#"]) "link")
                                      #:code 404))
 =>
 #"HTTP/1.1 404 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<a href=\"#\">link</a>"
 
 (write-response (response/xexpr '(a ([href "#"]) "link")
                                      #:message #"Bad request"))
 =>
 #"HTTP/1.1 200 Bad request\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<a href=\"#\">link</a>"
 
 (regexp-replace 
  #"Date: [a-zA-Z0-9:, ]+ GMT\r\n"
  (write-response (response/xexpr '(a ([href "#"]) "link")
                                       #:seconds 0)
                  #f)
  #"Date: REDACTED GMT\r\n")
 =>
 #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: Thu, 01 Jan 1970 00:00:00 GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<a href=\"#\">link</a>"
 
 (write-response (response/xexpr '(a ([href "#"]) "link")
                                      #:mime-type #"application/xml"))
 =>
 #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: application/xml\r\nConnection: close\r\n\r\n<a href=\"#\">link</a>"
 
 (write-response (response/xexpr '(a ([href "#"]) "link")
                                      #:headers (list (header #"head" #"value"))))
 =>
 #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\nhead: value\r\n\r\n<a href=\"#\">link</a>"
 
 (write-response (response/xexpr '(a ([href "#"]) "link")
                                      #:preamble #"<<!something XMLy>>"))
 =>
 #"HTTP/1.1 200 Okay\r\nDate: REDACTED GMT\r\nLast-Modified: REDACTED GMT\r\nServer: Racket\r\nContent-Type: text/html; charset=utf-8\r\nConnection: close\r\n\r\n<<!something XMLy>><a href=\"#\">link</a>")