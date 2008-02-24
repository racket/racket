#lang scheme/base
(require web-server/servlet)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)
(define (start initial-request)
  (parameterize ([current-url-transform
                  (lambda (k-url) "#")])
    (send/suspend
     (lambda (k-url)
       `(html (head (title "Hello"))
              (body (a ([href ,k-url])
                       "Link")))))))
