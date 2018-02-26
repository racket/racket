#lang racket/base
(require "input-port.rkt")

(provide prepare-change)

;; in atomic mode
;;  ... but may leave and return to atomic mode
(define (prepare-change in)
  (define prepare-change (core-input-port-prepare-change in))
  (when prepare-change
    (prepare-change)))
