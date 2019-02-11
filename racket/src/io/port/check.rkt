#lang racket/base
(require "../host/thread.rkt"
         "port.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "close.rkt")

(provide check-not-closed)

;; in atomic mode
;; Atomic mode is required on entry because an operation
;; that is prefixed when a port-closed check normally needs
;; to happen atomically with respect to the check.
(define (check-not-closed who cp)
  (when (core-port-closed? cp)
    (end-atomic)
    (define input? (core-input-port? cp))
    (raise-arguments-error who
                           (if input?
                               "input port is closed"
                               "output port is closed")
                           (if input?
                               "input port"
                               "output port")
                           cp)))
