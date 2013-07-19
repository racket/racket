#lang racket/base

(provide true false false?
         boolean=?
         symbol=?
         implies nand nor xor)
(require (for-syntax racket/base))

(define true #t)
(define false #f)

(define (false? v) (eq? v #f))

(define (boolean=? x y)
  (unless (and (boolean? x) (boolean? y))
    (raise-argument-error 'boolean=? "boolean?" (if (boolean? x) 1 0) x y))
  (eq? x y))

(define (symbol=? x y)
  (unless (and (symbol? x) (symbol? y))
    (raise-argument-error 'symbol=? "symbol?" (if (symbol? x) 1 0) x y))
  (eq? x y))

(define-syntax (implies stx)
  (syntax-case stx ()
    [(implies x y)
     (syntax/loc stx (if x y #t))]))

(define-syntax (nor stx)
  (syntax-case stx ()
    [(_ expr ...) (syntax/loc stx (not (or expr ...)))]))

(define-syntax (nand stx)
  (syntax-case stx ()
    [(_ expr ...) (syntax/loc stx (not (and expr ...)))]))

(define (xor a b)
  (if a
      (if b
          #f
          a)
      b))
