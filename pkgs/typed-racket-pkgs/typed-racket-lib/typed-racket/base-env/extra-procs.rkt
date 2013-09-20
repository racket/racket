#lang racket/base
(provide assert defined?)

(define-syntax assert
  (syntax-rules ()
    ((assert v)
     (or v (error (format "Assertion failed on ~v" v))))
    ((assert v pred)
     (let ((val v))
       (if ((#%expression pred) val)
           val
           (error (format "Assertion ~a failed on ~v" pred val)))))))

(define (defined? v)
  (not (equal? v (letrec ([x x]) x))))
