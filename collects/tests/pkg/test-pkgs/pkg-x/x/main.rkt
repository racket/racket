#lang racket/base
(require z)

(provide x)

(define (x) 
  (if (eq? (z) 'z) 'x 'ouch))

(module+ test
  (require y)
  (provide y-test)
  (define (y-test)
    (if (eq? (y) 'y) 'y 'ouch)))
