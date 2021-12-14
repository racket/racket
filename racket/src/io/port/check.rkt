#lang racket/base
(require "../host/thread.rkt"
         "../error/message.rkt"
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
    (raise
     (exn:fail
      (error-message->string
       who
       (string-append (if input?
                          "input port is closed"
                          "output port is closed")
                      "\n  "
                      (if input?
                          "input port: "
                          "output port: ")
                      ((error-value->string-handler) cp (error-print-width))))
      (current-continuation-marks)))))
