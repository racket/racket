#lang racket/base

(provide empty? cons?)

(define (empty? l) (null? l))
(define (cons? l) (pair? l))
