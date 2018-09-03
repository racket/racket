#lang racket/base
(require "../common/check.rkt"
         "host.rkt"
         "plumber.rkt")

(provide exit
         force-exit
         exit-handler)

(define/who exit-handler
  (make-parameter (let ([root-plumber (current-plumber)])
                    (lambda (v)
                      (plumber-flush-all root-plumber)
                      (force-exit v)))
                  (lambda (p)
                    (check who (procedure-arity-includes/c 1) p)
                    p)))

;; In a non-main place, must be called only in the scheduler:
(define (force-exit v)
  (cond
    [(byte? v)
     (host:exit v)]
    [else
     (host:exit 0)]))

(define (exit [v #t])
  ((exit-handler) v)
  (void))
