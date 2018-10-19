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
  (let ([write-out
         (let wo-loop ([p p])
           (let ([write-out (core-output-port-write-out (->core-output-port p))])
             (cond
               [(procedure? write-out) write-out]
               [else (wo-loop write-out)])))])
    (let loop ()
      (define r (atomically
                 (write-out #"" 0 0 #f #f #f)))
      (let r-loop ([r r])
        (cond
          [(eq? r 0) (void)]
          [(not r) (loop)]
          [(evt? r) (r-loop (sync r))]
          [else (error 'flush-output "weird result")])))))

;; ----------------------------------------

(define (maybe-flush-stdout in)
  (when (eq? in orig-input-port)
    (flush-output orig-output-port)
    (flush-output orig-error-port)))
