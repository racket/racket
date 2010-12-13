#lang racket/base
(require web-server/servlet-env)

(define (fac n a)
  (if (zero? n) a
      (fac (sub1 n) (* n a))))

(define (start req)
  (number->string (fac 10 1)))

(serve/servlet start
               #:servlet-regexp #rx""
               #:port 8000
               #:command-line? #t)
