#lang racket
(require (prefix-in uni: 2htdp/universe))

(define (server)
  (uni:universe 0
    (uni:on-new cons)
    (uni:on-msg list)
    (uni:on-tick add1)
    ;; Distinct from other tests:
    (uni:port 19204)))
