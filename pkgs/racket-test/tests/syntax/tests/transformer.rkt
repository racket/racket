#lang racket/base

(require rackunit
         (for-syntax racket/base syntax/transformer))

(define the-box (box add1))
(define-syntax op
  (make-variable-like-transformer
   #'(unbox the-box)
   #'(lambda (v) (set-box! the-box v))))
(check-equal? (op 5) 6)
(set! op 0)
(check-equal? op 0)
