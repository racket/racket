#lang racket/base

(provide make-print-config
         config-get)

;; Make a container for looking up parameters on-demand:
(define (make-print-config)
  (make-hasheq))

(define (config-get config param)
  (hash-ref config param (lambda ()
                           (define v (param))
                           (hash-set! config param v)
                           v)))

