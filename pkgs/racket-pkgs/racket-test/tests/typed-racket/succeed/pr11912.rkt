#lang typed/racket

(require typed/rackunit)

(check-exn (lambda (exn) #t)
           (lambda () (/ 1 0)))

(check-equal? 2 2)
(check-not-exn (lambda () (begin0 3 4)))

(check-true #t)
(check-false #f)
(check-not-false 4)
