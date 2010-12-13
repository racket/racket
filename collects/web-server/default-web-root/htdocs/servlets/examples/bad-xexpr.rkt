#lang web-server/insta
(define (start initial-request)
  (response/xexpr
   `(html (head (title "Foo"))
          (body (a ([href #f])
                   "Zog")))))
