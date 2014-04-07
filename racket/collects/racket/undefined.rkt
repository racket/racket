#lang racket/base

(provide undefined
         undefined?)

(define-values (struct:undef make-undef undef? undef-ref undef-set!)
  (make-struct-type 'undefined #f 0 0))
(define undefined (make-undef))

(define (undefined? v) (eq? v undefined))
