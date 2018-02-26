#lang racket/base
(require "syntax.rkt"
         "scope.rkt")

(provide syntax->list)

(define (syntax->list s)
  (define l
    (let loop ([s s])
      (cond
       [(pair? s) (cons (car s) (loop (cdr s)))]
       [(syntax? s) (loop (syntax-e s))]
       [else s])))
  (and (list? l)
       l))
