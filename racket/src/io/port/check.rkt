#lang racket/base
(require "../host/thread.rkt"
         "../error/message.rkt"
         "../error/value-string.rkt"
         "port.rkt"
         "lock.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "close.rkt")

(provide check-not-closed)

;; with p lock held or in atomic mode with lock requires atomic
;; ... and `unlock` required in the latter case
;; Some uninterrupted mode is required on entry because an operation
;; that is prefixed when a port-closed check normally needs
;; to happen atomically with respect to the check.
(define (check-not-closed who cp #:unlock [unlock #f])
  (when (core-port-closed? cp)
    (if unlock
        (unlock)
        (port-unlock cp))
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
                      (error-value->string cp)))
      (current-continuation-marks)))))
