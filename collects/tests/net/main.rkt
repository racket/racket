#lang scheme/base

(require tests/eli-tester
         (prefix-in ucodec:   "uri-codec.rkt")
         (prefix-in url:      "url.rkt")
         (prefix-in cgi:      "cgi.rkt")
         (prefix-in head:     "head.rkt")
         (prefix-in cookie:   "cookie.rkt")
         (prefix-in encoders: "encoders.rkt"))

(define (tests)
  (test do (begin (url:tests)
                  (ucodec:tests)
                  (cgi:tests)
                  (head:tests)
                  (cookie:tests)
                  (encoders:tests))))

(tests)
