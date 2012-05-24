#lang racket/base

(require tests/eli-tester
         (prefix-in ucodec:    "uri-codec.rkt")
         (prefix-in url:       "url.rkt")
         (prefix-in cgi:       "cgi.rkt")
         (prefix-in ftp:       "ftp.rkt")
         (prefix-in head:      "head.rkt")
         (prefix-in cookie:    "cookie.rkt")
         (prefix-in encoders:  "encoders.rkt")
         (prefix-in mime:      "mime.rkt")
         (prefix-in url-port:  "url-port.rkt")
         (prefix-in websocket: "websocket.rkt"))

(define (tests)
  (test do (url:tests)
        do (ucodec:tests)
        do (ucodec:noels-tests)
        do (cgi:tests)
        do (ftp:tests)
        do (head:tests)
        do (cookie:tests)
        do (encoders:tests)
        do (mime:tests)
        do (url-port:tests)
        do (websocket:tests)))

(tests)
