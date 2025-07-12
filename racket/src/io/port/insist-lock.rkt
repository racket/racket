#lang racket/base
(require "close.rkt")

(provide port-insist-atomic-lock)

(define (port-insist-atomic-lock p)
  ;; calling `port-closed-evt` has the side effect of
  ;; forcing the port's lock to be in atomic mode
  (port-closed-evt p)
  (void))
