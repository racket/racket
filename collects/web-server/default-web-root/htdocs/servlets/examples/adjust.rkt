#lang racket/base
(require web-server/servlet)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)
(define (start initial-request)
  (adjust-timeout! 1)
  (send/suspend
   (lambda (k-url)
     (response/xexpr
      `(html (head (title "Hello"))
             (body (a ([href ,k-url])
                      "Link"))))))
  (response/xexpr
   `(html (body "Should not happen"))))
