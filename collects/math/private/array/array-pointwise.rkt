#lang racket/base

(require typed/untyped-utils
         racket/math
         (rename-in "typed-array-pointwise.rkt"
                    [array-map  typed:array-map])
         (rename-in "untyped-array-pointwise.rkt"
                    [array-map  untyped:array-map]))

(define-typed/untyped-identifier array-map
  typed:array-map untyped:array-map)

(define-syntax-rule (define-array-op1 name op)
  (define-syntax-rule (name arr) (array-map op arr)))

(define-syntax-rule (define-array-op2 name op)
  (define-syntax-rule (name arr0 arr1) (array-map op arr0 arr1)))

(define-syntax-rule (define-array-op1+ name op)
  (define-syntax-rule (name arr0 arrs (... ...)) (array-map op arr0 arrs (... ...))))

(define-syntax-rule (define-array-op2+ name op)
  (define-syntax-rule (name arr0 arr1 arrs (... ...)) (array-map op arr0 arr1 arrs (... ...))))

(define-syntax-rule (define-array-op name op)
  (define-syntax-rule (name arrs (... ...)) (array-map op arrs (... ...))))

(define-syntax-rule (array-scale arr x-expr)
  (let ([x x-expr])
    (inline-array-map (λ (y) (* x y)) arr)))

(define-array-op1 array-sqr sqr)
(define-array-op1 array-sqrt sqrt)
(define-array-op1 array-abs abs)
(define-array-op1 array-magnitude magnitude)
(define-array-op1 array-angle angle)
(define-array-op1 array-conjugate conjugate)
(define-array-op1 array-real-part real-part)
(define-array-op1 array-imag-part imag-part)

(define-array-op2 array-make-rectangular make-rectangular)
(define-array-op2 array-make-polar make-polar)

(define-array-op array+ +)
(define-array-op array* *)
(define-array-op1+ array- -)
(define-array-op1+ array/ /)

(define-array-op1+ array-min min)
(define-array-op1+ array-max max)

(define-array-op2+ array< <)
(define-array-op2+ array<= <=)
(define-array-op2+ array> >)
(define-array-op2+ array>= >=)
(define-array-op2+ array= =)

(define-array-op2 array-not not)

(define-syntax-rule (array-and arrs ...) (inline-array-map and arrs ...))
(define-syntax-rule (array-or arrs ...) (inline-array-map or arrs ...))
(define-syntax-rule (array-if arr0 arr1 arr2) (inline-array-map if arr0 arr1 arr2))

(provide
 ;; Mapping
 inline-array-map
 array-map
 ;; Lifted operators
 array-scale
 array-sqr
 array-sqrt
 array-abs
 array-magnitude
 array-angle
 array-conjugate
 array-real-part
 array-imag-part
 array-make-rectangular
 array-make-polar
 array+
 array-
 array*
 array/
 array-min
 array-max     
 array=
 array<
 array<=
 array>
 array>=
 array-not
 array-and
 array-or
 array-if)
