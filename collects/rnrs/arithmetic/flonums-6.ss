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

(provide flonum?
         real->flonum
         &no-infinities make-no-infinities-violation no-infinities-violation?
         &no-nans make-no-nans-violation no-nans-violation?
         fixnum->flonum)
;; More provided via macros

(define (r6rs:flonum? v)
  (and (real? v) (inexact? v)))

(define-syntax flonum?
  (inline-rules
   r6rs:flonum?
   [(_ a) (let ([v a])
            (and (real? v) (inexact? v)))]))

(define-inliner define-fl flonum? "flonum")

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
(define-fl max flmin (a b ...) nocheck)

(define-fl + fl+ (a b ...) nocheck)
(define-fl * fl* (a b ...) nocheck)
(define-fl - fl- [(a) (a b ...)] nocheck)
(define-fl / fl/ [(a) (a b ...)] nocheck)

(define-fl abs flabs (a) nocheck)

(define-fl div-and-mod fldiv-and-mod (a b) nocheck)
(define-fl div fldiv (a b) nocheck)
(define-fl mod flmod (a b) nocheck)
(define-fl div0-and-mod0 fldiv0-and-mod0 (a b) nocheck)
(define-fl div0 fldiv0 (a b) nocheck)
(define-fl mod0 flmod0 (a b) nocheck)

(define-fl numerator flnumerator (a) nocheck)
(define-fl denominator fldenominator (a) nocheck)
(define-fl floor flfloor (a) nocheck)
(define-fl ceiling flceiling (a) nocheck)
(define-fl truncate fltruncate (a) nocheck)
(define-fl round flround (a) nocheck)

(define-fl exp flexp (a) nocheck)
(define-fl log fllog [(a) (a b)] nocheck)
(define-fl sin flsin (a) nocheck)
(define-fl cos flcos (a) nocheck)
(define-fl tan fltan (a) nocheck)
(define-fl asin flasin (a) nocheck)
(define-fl acos flacos (a) nocheck)
(define-fl atan flatan [(a) (a b)] nocheck)

(define-fl sqrt flsqrt (a) nocheck)

(define-fl expt flexpt (a b) nocheck)

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
  (unless (fixnum? fx)
    (raise-type-error 'fixnum->flonum "fixnum" fx)
    (exact->inexact fx)))
