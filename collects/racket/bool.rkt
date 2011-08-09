#lang scheme/base

(provide true false false?
         boolean=?
         symbol=?)

(define true #t)
(define false #f)

(define (false? v) (eq? v #f))

(define (boolean=? x y)
  (unless (and (boolean? x) (boolean? y))
    (raise-type-error 'boolean=? "boolean" (if (boolean? x) 1 0) x y))
  (eq? x y))

(define (symbol=? x y)
  (unless (and (symbol? x) (symbol? y))
    (raise-type-error 'symbol=? "symbol" (if (symbol? x) 1 0) x y))
  (eq? x y))

