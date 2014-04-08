#lang racket/base

(provide undefined)

(define-values (struct:undef make-undef undef? undef-ref undef-set!)
  (make-struct-type 'undefined #f 0 0))
(define undefined (make-undef))
