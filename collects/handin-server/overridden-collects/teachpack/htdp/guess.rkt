#lang racket/gui

(require htdp/error lang/prim)

(provide guess-with-gui guess-with-gui-3 guess-with-gui-list)

(define-higher-order-primitive guess-with-gui guess-with-gui/proc
  (check-guess))
(define-higher-order-primitive guess-with-gui-3 guess-with-gui-3/proc
  (check-guess))
(define-higher-order-primitive guess-with-gui-list guess-with-gui-list/proc
  (_ check-guess-list))

(define (guess-with-gui/proc cg)
  (check-proc 'guess-with-gui cg 2 'first "two arguments")
  (void))

(define (guess-with-gui-3/proc cg)
  (check-proc 'guess-with-gui-3 cg (+ 3 1) 'first "four arguments")
  (void))

(define (guess-with-gui-list/proc n cg)
  (check-arg  'guess-with-gui-list
              (and (number? n) (integer? n) (>= n 1)) "positive integer" '1st n)
  (check-proc 'guess-with-gui-list cg 2 'first "two arguments")
  (unless  (<= (expt 10 n) 2147483647)
    (error 'guess-with-gui-list "the given number of digits (~a) is too large" n))
  (void))
