#lang racket/base
(require racket/place)

(provide go)

(define (go ch)
  (place-channel-put ch 42))
