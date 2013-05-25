#lang racket/base
(require web-server/servlet)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)
(define (start initial-request)
  (parameterize ([current-servlet-continuation-expiration-handler
                  (lambda _
                    (response/xexpr
                     `(html (body "Expired"))))])
    (send/suspend (lambda (k-url) (response/xexpr `(html (a ([href ,k-url]) "Link")))))
    (send/forward (lambda (k-url) (response/xexpr `(html (a ([href ,k-url]) "Link")))))
    (send/finish (response/xexpr `(html (body "Done."))))))
