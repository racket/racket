#lang racket/base
(require z)

(provide x)

(define (x) 
  (if (eq? (z) 'z) 'x 'ouch))
