#;
(exn-pred #rx"could not convert type to a contract.*Async-Channelof")
#lang racket/load

;; Test typed-untyped interaction with channels

(module typed typed/racket
  (require typed/racket/async-channel)
  (: ch (Async-Channelof (Boxof Integer)))
  (define ch (make-async-channel))
  (: putter (-> Thread))
  (define (putter)
    (thread (λ () (async-channel-put ch (box 3)))))
  (provide putter ch))

(require 'typed)
(putter)
(set-box! (async-channel-get ch) "not an integer")

