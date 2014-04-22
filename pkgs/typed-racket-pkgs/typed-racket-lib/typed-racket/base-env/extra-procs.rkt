#lang racket/base
(provide assert defined?)

(define-syntax assert
  (syntax-rules ()
    ((assert v)
     (let ([val v])
       (or val (error (format "Assertion failed on ~v" val)))))
    ((assert v pred)
     (let ((val v))
       (if ((#%expression pred) val)
           val
           (error (format "Assertion ~a failed on ~v" pred val)))))))

(define (defined? v) #t)
