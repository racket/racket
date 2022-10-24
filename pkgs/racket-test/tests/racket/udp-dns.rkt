#lang racket/base
(require racket/udp)

;; Regression test to check that name-resolution failure doesn't finish in atomic mode
;; and crash on `(sleep)`

(with-handlers ([exn? displayln])
  (udp-send-to* (udp-open-socket) "this_does_not_resolve.example.com" 1234 #""))

(sleep)
