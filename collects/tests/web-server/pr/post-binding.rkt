#lang racket/base
(require web-server/http
         web-server/http/request
         web-server/http/bindings
         rackunit
         "../util.rkt")

(define request-bs
  #"POST /xapps/agent-stat HTTP/1.1\r\nUser-Agent: curl/7.21.6 (x86_64-pc-linux-gnu) libcurl/7.21.6 OpenSSL/1.0.0e zlib/1.2.3.4 libidn/1.22 librtmp/2.3\r\nHost: localhost:8080\r\nAccept: */*\r\nContent-Length: 25\r\nContent-Type: application/x-www-form-urlencoded\r\n\r\nstat=testing%20via%20curl")

(define-values (conn ip op)
  (make-mock-connection request-bs))
(define-values (req close?)
  (read-request conn 80 (λ _ (values "to" "from"))))
(check-equal? (request-bindings req)
              (list (cons 'stat "testing via curl")))
