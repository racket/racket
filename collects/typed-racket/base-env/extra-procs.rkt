#lang racket/base
(provide assert defined?)

(define-syntax assert
  (syntax-rules ()
    ((assert v)
     (or v (error "Assertion failed")))
    ((assert v pred)
     (let ((val v))
       (if (pred val) val (error "Assertion failed"))))))

(define (defined? v)
  (not (equal? v (letrec ([x x]) x))))
