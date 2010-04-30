#lang scheme/base

(require tests/eli-tester
         (prefix-in ucodec:   "uri-codec.ss")
         (prefix-in url:      "url.ss")
         (prefix-in cgi:      "cgi.ss")
         (prefix-in head:     "head.ss")
         (prefix-in cookie:   "cookie.ss")
         (prefix-in encoders: "encoders.ss"))

(define (tests)
  (test do (begin (url:tests)
                  (ucodec:tests)
                  (cgi:tests)
                  (head:tests)
                  (cookie:tests)
                  (encoders:tests))))

(tests)
