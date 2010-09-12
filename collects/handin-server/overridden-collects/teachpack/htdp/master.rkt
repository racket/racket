#lang racket/gui

(require htdp/error lang/prim)

(provide master)

(define-higher-order-primitive master master/proc (compare-guess))

(define (master/proc cg)
  (check-proc 'master cg 4 'first 'arguments)
  (void))
