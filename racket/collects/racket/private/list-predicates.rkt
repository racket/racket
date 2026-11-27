#lang racket/base
(#%declare #:unsafe-cross-phase-persistent)

(provide empty? cons?)

(define (empty? l) (null? l))
(define (cons? l) (pair? l))
