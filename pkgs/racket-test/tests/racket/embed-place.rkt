#lang racket/base

;; This module is used by "embed-in-c.rkt"

(require racket/place)

(provide go)

(define (go ch)
  (place-channel-put ch 42))
