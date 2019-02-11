#lang racket/base
(require "../common/class.rkt"
         "port.rkt"
         "input-port.rkt")

(provide prepare-change)

;; in atomic mode
;;  ... but may leave and return to atomic mode
(define (prepare-change in)
  (define prepare-change (method core-input-port in prepare-change))
  (when prepare-change
    (prepare-change in)))
