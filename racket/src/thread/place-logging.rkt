#lang racket/base
(require "host.rkt"
         "place-object.rkt")

(provide log-place

         install-place-logging-procs!)

(struct place-event (id action data time)
  #:prefab)

(define (log-place msg
                   #:action [action (string->symbol msg)]
                   #:data [data #f])
  (when (logging-place-events?)
    (define id (place-id current-place))
    (log-place-event (string-append
                      "id "
                      (number->string id)
                      ": "
                      msg
                      (if data
                          (string-append " " (number->string data))
                          ""))
                     (place-event id action data (current-inexact-milliseconds)))))

;; ----------------------------------------

(define logging-place-events? (lambda () #f))
(define log-place-event (lambda (msg e) (void)))

(define (install-place-logging-procs! logging? log)
  (set! logging-place-events? logging?)
  (set! log-place-event log))
