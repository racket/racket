#lang racket/base
(require "../common/check.rkt"
         "../common/class.rkt"
         "../host/thread.rkt"
         "parameter.rkt"
         "port.rkt"
         "output-port.rkt"
         "pipe.rkt"
         "check.rkt"
         "fd-port.rkt")

(provide flush-output
         maybe-flush-stdout)

(define/who (flush-output [p (current-output-port)])
  (check who output-port? p)
  (let wo-loop ([p p])
    (define out (->core-output-port p))
    (define write-out (method core-output-port out write-out))
    (cond
      [(procedure? write-out)
       (let loop ()
         (start-atomic)
         (check-not-closed who out)
         (define r (write-out out #"" 0 0 #f #f #f))
         (end-atomic)
         (let r-loop ([r r])
           (cond
             [(eq? r 0) (void)]
             [(not r) (loop)]
             [(evt? r) (r-loop (sync r))]
             [else (error 'flush-output "weird result")])))]
      [else
       (atomically (check-not-closed who out))
       (wo-loop write-out)])))

;; ----------------------------------------

(define (maybe-flush-stdout in)
  (when (eq? in orig-input-port)
    (when (terminal-port? orig-output-port)
      (flush-output orig-output-port))
    (when (terminal-port? orig-error-port)
      (flush-output orig-error-port))))
