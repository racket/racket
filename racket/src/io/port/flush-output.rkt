#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "parameter.rkt"
         "output-port.rkt"
         "pipe.rkt")

(provide flush-output
         maybe-flush-stdout)

(define/who (flush-output [p (current-output-port)])
  (check who output-port? p)
  (let ([p (->core-output-port p)])
    (let loop ()
      (define r (atomically
                 ((core-output-port-write-out p) #"" 0 0 #f #f #f)))
      (let r-loop ([r r])
        (cond
          [(eq? r 0) (void)]
          [(not r) (loop)]
          [(evt? r) (r-loop (sync r))]
          [else (error 'flush-output "weird result")])))))

;; ----------------------------------------

(define orig-input-port (current-input-port))
(define orig-output-port (current-output-port))
(define orig-error-port (current-error-port))

(define (maybe-flush-stdout in)
  (when (eq? in orig-input-port)
    (flush-output orig-output-port)
    (flush-output orig-error-port)))
