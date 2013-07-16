#lang racket
(require net/url
         rackunit
         tests/eli-tester
         htdp/bsl/reader
         web-server/servlet/web)
(require/expose web-server/servlet/web
                (embed-ids))
(require/expose htdp/bsl/reader
                (wrap-reader))

(define base-url
  (string->url
   "http://localhost:8000/servlets/standalone.rkt"))
(define a-k-url
  (string->url
   "http://localhost:8000/servlets/standalone.rkt;((%22k%22%20.%20%22(1%201%2033620206)%22))"))
(define k-info
  '(1 1 33620206))

(define (can-encode?)
  (test
   (continuation-url? a-k-url) => k-info
   (embed-ids k-info base-url) => (url->string a-k-url)))

(test
 (can-encode?)
 ((wrap-reader can-encode? empty)))
