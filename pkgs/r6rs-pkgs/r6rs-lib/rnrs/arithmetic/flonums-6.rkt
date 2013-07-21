#lang scheme/base

(require (only-in rnrs/base-6
                  div-and-mod div mod
                  div0-and-mod0 div0 mod0
                  [integer? r6rs:integer?]
                  finite? infinite? nan?)
         (prefix-in core: scheme/flonum)
         scheme/fixnum
         (only-in rnrs/arithmetic/fixnums-6
                  fixnum?)
         rnrs/conditions-6
         r6rs/private/num-inline)

(provide (rename-out [inexact-real? flonum?])
         real->flonum
         flnumerator
         fldenominator
         fllog (rename-out [core:flsqrt flsqrt]) flexpt
         &no-infinities make-no-infinities-violation no-infinities-violation?
         &no-nans make-no-nans-violation no-nans-violation?)
;; More provided via macros

(define-inliner define-fl inexact-real? "flonum")

(define-fl = fl=? core:fl= (a b c ...) nocheck)
(define-fl > fl>? core:fl> (a b c ...) nocheck)
(define-fl < fl<? core:fl< (a b c ...) nocheck)
(define-fl <= fl<=? core:fl<= (a b c ...) nocheck)
(define-fl >= fl>=? core:fl>= (a b c ...) nocheck)

(define-fl integer? flinteger? #f (a) nocheck)
(define-fl zero? flzero? #f (a) nocheck)
(define-fl positive? flpositive? #f (a) nocheck)
(define-fl negative? flnegative? #f (a) nocheck)
(define-fl odd? flodd? #f (a) nocheck)
(define-fl even? fleven? #f (a) nocheck)
(define-fl finite? flfinite? #f (a) nocheck)
(define-fl infinite? flinfinite? #f (a) nocheck)
(define-fl nan? flnan? #f (a) nocheck)

(define-fl max flmax core:flmax (a b ...) nocheck)
(define-fl min flmin core:flmin (a b ...) nocheck)

(define-fl + fl+ core:fl+ #:base 0.0 (a b ...) nocheck)
(define-fl * fl* core:fl* #:base 1.0 (a b ...) nocheck)
(define-fl - fl- core:fl- [(a) (a b ...)] nocheck)
(define-fl / fl/ core:fl/ [(a) (a b ...)] nocheck)

(define-fl abs flabs core:flabs (a) nocheck)

(provide fldiv-and-mod
         fldiv0-and-mod0)
(define (fldiv-and-mod a b)
  (unless (inexact-real? a)
    (raise-type-error 'fldiv-and-mod "flonum" a))
  (unless (inexact-real? b)
    (raise-type-error 'fldiv-and-mod "flonum" b))
  (div-and-mod a b))
(define-fl div fldiv #f (a b) nocheck)
(define-fl mod flmod #f (a b) nocheck)
(define (fldiv0-and-mod0 a b)
  (unless (inexact-real? a)
    (raise-type-error 'fldiv0-and-mod0 "flonum" a))
  (unless (inexact-real? b)
    (raise-type-error 'fldiv0-and-mod0 "flonum" b))
  (div0-and-mod0 a b))
(define-fl div0 fldiv0 #f (a b) nocheck)
(define-fl mod0 flmod0 #f (a b) nocheck)

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

(provide (rename-out [core:flfloor flfloor]
                     [core:flceiling flceiling]
                     [core:flround flround]
                     [core:fltruncate fltruncate]
                     [core:flexp flexp]))

(define fllog
  (case-lambda
   [(v) (core:fllog v)]
   [(v1 v2)
    (/ (fllog v1) (fllog v2))]))

(provide (rename-out [core:flsin flsin]
                     [core:flcos flcos]
                     [core:fltan fltan]
                     [core:flasin flasin]
                     [core:flacos flacos]))

(define-fl atan flatan #f [(a) (a b)] nocheck)

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

(provide (rename-out [fx->fl fixnum->flonum]))
