#lang racket/base

(provide port-number?
         listen-port-number?)

(define (port-number? v)
  (and (fixnum? v)
       (<= 1 v 65535)))

(define (listen-port-number? v)
  (and (fixnum? v)
       (<= 0 v 65535)))
