#lang racket/base
(require rackunit
         net/url
         web-server/servlet/web)
(require/expose web-server/servlet/web
                (embed-ids))

(provide web-tests)

(define url0 (string->url "http://test.com/servlets/example.rkt"))

(define web-tests
  (test-suite
   "Web"
   
   (test-suite 
    "continuation-url? and embed-ids"
    (test-false "not k-url" (continuation-url? url0))
    (test-equal? "identity"
                 (continuation-url? (string->url (embed-ids (list 1 2 3) url0)))
                 (list 1 2 3)))))
