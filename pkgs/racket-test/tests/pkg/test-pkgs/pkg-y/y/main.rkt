#lang racket/base

(provide y)

(define (y) 
  (if (zero? (random 1)) 'y 'ouch))