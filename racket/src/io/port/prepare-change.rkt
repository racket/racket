#lang racket/base
(require "../common/class.rkt"
         "port.rkt"
         "input-port.rkt"
         "lock.rkt")

(provide prepare-change)

;; with lock held
;;  ... but may leave and return to holding the lock
(define (prepare-change in)
  (define prepare-change (method core-input-port in prepare-change))
  (when prepare-change
    (prepare-change in)))
