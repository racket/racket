#lang plai/web

(define-type A
  [mta])

(define (start req)
  (response/xexpr
   "Hello"))
