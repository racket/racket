#lang scheme/base

(require (only-in rnrs/base-6
                  div-and-mod div mod
                  div0-and-mod0 div0 mod0
                  [integer? r6rs:integer?]
                  finite? infinite? nan?)
         (only-in rnrs/arithmetic/fixnums-6
                  fixnum?)
         rnrs/conditions-6
         r6rs/private/num-inline
         (for-syntax r6rs/private/inline-rules))

(provide (rename-out [inexact-real? flonum?])
         real->flonum
         flnumerator
         fldenominator
         fllog flsqrt flexpt
         &no-infinities make-no-infinities-violation no-infinities-violation?
         &no-nans make-no-nans-violation no-nans-violation?
         fixnum->flonum)
;; More provided via macros

(define-inliner define-fl inexact-real? "flonum")

(define-fl = fl=? (a b c ...) nocheck)
(define-fl > fl>? (a b c ...) nocheck)
(define-fl < fl<? (a b c ...) nocheck)
(define-fl <= fl<=? (a b c ...) nocheck)
(define-fl >= fl>=? (a b c ...) nocheck)

(define-fl integer? flinteger? (a) nocheck)
(define-fl zero? flzero? (a) nocheck)
(define-fl positive? flpositive? (a) nocheck)
(define-fl negative? flnegative? (a) nocheck)
(define-fl odd? flodd? (a) nocheck)
(define-fl even? fleven? (a) nocheck)
(define-fl finite? flfinite? (a) nocheck)
(define-fl infinite? flinfinite? (a) nocheck)
(define-fl nan? flnan? (a) nocheck)

(define-fl max flmax (a b ...) nocheck)
(define-fl min flmin (a b ...) nocheck)

(define-fl + fl+ (a b ...) nocheck)
(define-fl * fl* (a b ...) nocheck)
(define-fl - fl- [(a) (a b ...)] nocheck)
(define-fl / fl/ [(a) (a b ...)] nocheck)

(define-fl abs flabs (a) nocheck)

(provide fldiv-and-mod
         fldiv0-and-mod0)
(define (fldiv-and-mod a b)
  (unless (inexact-real? a)
    (raise-type-error 'fldiv-and-mod "flonum" a))
  (unless (inexact-real? b)
    (raise-type-error 'fldiv-and-mod "flonum" b))
  (div-and-mod a b))
(define-fl div fldiv (a b) nocheck)
(define-fl mod flmod (a b) nocheck)
(define (fldiv0-and-mod0 a b)
  (unless (inexact-real? a)
    (raise-type-error 'fldiv0-and-mod0 "flonum" a))
  (unless (inexact-real? b)
    (raise-type-error 'fldiv0-and-mod0 "flonum" b))
  (div0-and-mod0 a b))
(define-fl div0 fldiv0 (a b) nocheck)
(define-fl mod0 flmod0 (a b) nocheck)

(define (flnumerator c)
  (if (inexact-real? c)
      (if (and (rational? c)
               (not (equal? c -0.0)))
          (numerator c)
          c)
      (raise-type-error 'flnumerator "flonum" c)))

(define (fldenominator c)
  (if (inexact-real? c)
      (if (rational? c)
          (denominator c)
          1.0)
      (raise-type-error 'fldenominator "flonum" c)))

(define-fl floor flfloor (a) nocheck)
(define-fl ceiling flceiling (a) nocheck)
(define-fl truncate fltruncate (a) nocheck)
(define-fl round flround (a) nocheck)

(define-fl exp flexp (a) nocheck)

(define fllog
  (case-lambda
   [(v) 
    (unless (inexact-real? v)
      (raise-type-error 'fllog "flonum" v))
    (let ([v (log v)])
      (if (inexact-real? v)
          v
          +nan.0))]
   [(v1 v2)
    (/ (fllog v1) (fllog v2))]))

(define-fl sin flsin (a) nocheck)
(define-fl cos flcos (a) nocheck)
(define-fl tan fltan (a) nocheck)
(define-fl asin flasin (a) nocheck)
(define-fl acos flacos (a) nocheck)
(define-fl atan flatan [(a) (a b)] nocheck)

(define (flsqrt v)
  (unless (inexact-real? v)
    (raise-type-error 'flsqrt "flonum" v))
  (let ([v (sqrt v)])
    (if (inexact-real? v)
        v
        +nan.0)))

(define (flexpt a b)
  (unless (inexact-real? a)
    (raise-type-error 'flexpt "flonum" a))
  (unless (inexact-real? b)
    (raise-type-error 'flexpt "flonum" b))
  (let ([v (expt a b)])
    (if (inexact-real? v)
        v
        +nan.0)))

(define-condition-type &no-infinities
  &implementation-restriction
  make-no-infinities-violation
  no-infinities-violation?)

(define-condition-type &no-nans
  &implementation-restriction
  make-no-nans-violation no-nans-violation?)

(define (real->flonum r)
  (unless (real? r)
    (raise-type-error 'real->flonum "real" r))
  (exact->inexact r))

(define (fixnum->flonum fx)
  (if (fixnum? fx)
      (exact->inexact fx)
      (raise-type-error 'fixnum->flonum "fixnum" fx)))
