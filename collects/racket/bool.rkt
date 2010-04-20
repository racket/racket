#lang scheme/base

(provide true false false?
         boolean=?
         symbol=?)

(define-syntax-rule (define-constant id val)
  (...
   (define-syntax id
     (syntax-id-rules (set!)
       [(set! id rhs) (set! val rhs)]
       [(id . args) (val . args)]
       [_ val]))))

(define-constant true #t)
(define-constant false #f)

(define (false? v) (eq? v #f))

(define (boolean=? x y)
  (unless (and (boolean? x) (boolean? y))
    (raise-type-error 'boolean=? "boolean" (if (boolean? x) 1 0) x y))
  (eq? x y))

(define (symbol=? x y)
  (unless (and (symbol? x) (symbol? y))
    (raise-type-error 'symbol=? "symbol" (if (symbol? x) 1 0) x y))
  (eq? x y))

