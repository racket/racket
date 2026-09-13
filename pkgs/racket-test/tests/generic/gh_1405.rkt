#lang racket/base

(require racket/generic
         rackunit)

(define-generics thing
  (foo thing x)
  #:fallbacks
  [(define (foo self x) x)]
  #:derive-property prop:procedure (lambda (self x) (foo self x)))

(struct Bar ()
  #:methods gen:thing [])

(check-equal? ((Bar) 'x) 'x)