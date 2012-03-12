#lang racket/base
(require racket/contract
         web-server/servlet/servlet-structs)

(provide/contract
 [make-servlet-tester
  (-> (-> request?
          can-be-response?)
      (->* ()
           ((or/c string? url? request? false/c)
            (listof binding?)
            #:raw? boolean?)
           (or/c bytes?
                 xexpr?)))])

; Real Library
(require racket/list
         racket/promise
         net/url
         web-server/http
         web-server/servlet-dispatch)

(define (make-servlet-tester servlet)
  (define d (dispatch/servlet servlet))
  (λ ([s-or-u-or-req #f] 
      [bs empty]
      #:raw? [raw? #f])
    (define req
      (if (request? s-or-u-or-req)
          s-or-u-or-req
          (let ()
            (define s-or-u
              (if s-or-u-or-req
                  s-or-u-or-req
                  ""))
            (define u 
              (if (string? s-or-u)
                  (string->url s-or-u)
                  s-or-u))
            (make-request #"GET" u empty (delay bs) #"" "127.0.0.1" 80 "127.0.0.1"))))
    (call d req #:raw? raw?)))

; Intermediate Library
(require racket/match
         xml
         web-server/private/timer
         web-server/private/connection-manager
         web-server/private/web-server-structs)

(define (call d req #:raw? raw?)
  (htxml (collect d req) raw?))
(define (htxml bs raw?)
  (match (regexp-match #"^.+\r\n\r\n(.*)$" bs)
    [(list _ s)
     (if raw?
         s
         (string->xexpr (bytes->string/utf-8 s)))]
    [_
     (error 'servlet "Servlet did not return an HTTP response, instead returned ~v"
            bs)]))

(define (collect d req)
  (parameterize ([current-custodian (make-custodian)])
    (define-values (c i o) (make-mock-connection #""))
    (parameterize ([current-server-custodian (current-custodian)])
      (call-with-continuation-barrier
       (lambda ()
         (d c req))))
    (redact (get-output-bytes o))))

(define (make-mock-connection ib)
  (define ip (open-input-bytes ib))
  (define op (open-output-bytes))
  (values (make-connection 0 (make-timer never-evt +inf.0 (lambda () (void)))
                           ip op (current-custodian) #t)
          ip
          op))

(define (redact b)
  (regexp-replace 
   #"Date: [a-zA-Z0-9:, ]+ GMT\r\n"
   (regexp-replace
    #"Last-Modified: [a-zA-Z0-9:, ]+ GMT\r\n"
    b
    #"Last-Modified: REDACTED GMT\r\n")
   #"Date: REDACTED GMT\r\n"))
