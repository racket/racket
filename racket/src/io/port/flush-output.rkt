#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "parameter.rkt"
         "port.rkt"
         "output-port.rkt"
         "pipe.rkt")

(provide flush-output
         maybe-flush-stdout)

(define/who (flush-output [p (current-output-port)])
  (check who output-port? p)
  (let wo-loop ([p p])
    (let ([write-out (core-output-port-write-out (->core-output-port p))])
      (cond
        [(procedure? write-out)
         (let loop ()
           (define r (atomically
                      (write-out (core-port-self p) #"" 0 0 #f #f #f)))
           (let r-loop ([r r])
             (cond
               [(eq? r 0) (void)]
               [(not r) (loop)]
               [(evt? r) (r-loop (sync r))]
               [else (error 'flush-output "weird result")])))]
        [else (wo-loop write-out)]))))

;; ----------------------------------------

(define (maybe-flush-stdout in)
  (when (eq? in orig-input-port)
    (flush-output orig-output-port)
    (flush-output orig-error-port)))
