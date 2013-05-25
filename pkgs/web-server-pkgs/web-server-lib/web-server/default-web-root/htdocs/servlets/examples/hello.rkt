#lang racket/base
(require web-server/http)
(provide (all-defined-out))
(define interface-version 'v1)
(define timeout +inf.0)
(define (start initial-request)
  (define the-text "Hello, Web!")
  (response/xexpr
   `(html (head (title ,the-text))
          (body ([bgcolor "white"])
                (p ,the-text)))))
