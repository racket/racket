#lang racket/base

(provide defn?
         defn-syms
         defn-rhs)

(define (defn? e)
  (and (pair? e)
       (eq? (car e) 'define-values)))
(define defn-syms cadr)
(define defn-rhs caddr)

