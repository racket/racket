#lang typed/racket/base

(require "flonum.rkt"
         "private/exception.rkt")

(provide
;; Extended integers and rationals
 Float-Not-Rational flonum-not-rational?
 Extended-Integer extended-integer?
 Extended-Rational extended-rational?
 real->extended-integer
 real->extended-rational
 ext-abs ext+ ext- ext* ext/ ext-min ext-max ext= ext< ext<= ext> ext>=)

;; ===================================================================================================
;; Extended integers and rationals

(define-type Float-Not-Rational (U +inf.0 -inf.0 Float-Nan))
(define-type Extended-Integer (U Integer Float-Not-Rational))
(define-type Extended-Rational (U Exact-Rational Float-Not-Rational))

(define-predicate flonum-not-rational? Float-Not-Rational)
(define-predicate extended-integer? Extended-Integer)
(define-predicate extended-rational? Extended-Rational)

(: real->extended-integer (Real -> Extended-Integer))
(define (real->extended-integer x)
  (let ([x  (if (single-flonum? x) (fl x) x)])
    (cond [(integer? x)  (let ([x  (inexact->exact x)])
                            (if (integer? x) x +nan.0))]
          [(flonum-not-rational? x)  x]
          [else  +nan.0])))

(: real->extended-rational (Real -> Extended-Rational))
(define (real->extended-rational x)
  (let ([x  (if (single-flonum? x) (fl x) x)])
    (cond [(rational? x)  (inexact->exact x)]
          [(flonum-not-rational? x)  x]
          [else  +nan.0])))

(: flonum->flonum-not-rational (Float -> Float-Not-Rational))
(define (flonum->flonum-not-rational x)
  (cond [(flonum-not-rational? x)  x]
        [else  (raise-argument-error 'flonum->flonum-not-rational "Float-Not-Rational" x)]))

(define-syntax-rule (make-extended-unary-op op flop fl->return-type)
  (λ (x)
    (cond [(flonum-not-rational? x)
           (fl->return-type (flop x))]
          [else  (op x)])))

(: ext-abs (case-> (Extended-Integer -> Extended-Integer)
                   (Extended-Rational -> Extended-Rational)))
(define ext-abs (make-extended-unary-op abs flabs flonum->flonum-not-rational))

(define-syntax-rule (make-extended-binary-op op flop exact->fl fl->return-type)
  (λ (x y)
    (cond [(or (flonum? x) (flonum? y))
           (let ([x  (exact->fl x)] [y  (exact->fl y)])
             (fl->return-type (flop x y)))]
          [else  (op x y)])))

(: ext+ (case-> (Extended-Integer Extended-Integer -> Extended-Integer)
                (Extended-Rational Extended-Rational -> Extended-Rational)))
(define ext+
  (make-extended-binary-op
   + fl+
   (λ (x) (if (flonum? x) x 0.0))
   flonum->flonum-not-rational))

(: ext- (case-> (Extended-Integer Extended-Integer -> Extended-Integer)
                (Extended-Rational Extended-Rational -> Extended-Rational)))
(define ext-
  (make-extended-binary-op
   - fl-
   (λ (x) (if (flonum? x) x 0.0))
   flonum->flonum-not-rational))

(: ext* (case-> (Extended-Integer Extended-Integer -> Extended-Integer)
                (Extended-Rational Extended-Rational -> Extended-Rational)))
(define ext*
  (make-extended-binary-op
   * fl*
   (λ (x) (if (flonum? x) x (flsgn (fl x))))
   flonum->flonum-not-rational))

(: ext/ (Extended-Rational Extended-Rational -> Extended-Rational))
(define (ext/ x y)
  (cond [(zero? x)  (if (or (positive? y) (negative? y)) 0 +nan.0)]
        [(zero? y)  (cond [(positive? x)  +inf.0]
                          [(negative? x)  -inf.0]
                          [else  +nan.0])]
        [(or (flonum? x) (flonum? y))
         (let ([x  (if (flonum? x) x (flsgn (fl x)))]
               [y  (if (flonum? y) y (flsgn (fl y)))])
           (real->extended-rational (fl/ x y)))]
        [else  (/ x y)]))

(: ext-min (case-> (Extended-Integer Extended-Integer -> Extended-Integer)
                   (Extended-Rational Extended-Rational -> Extended-Rational)))
(define (ext-min x y)
  (cond [(and (flonum? x) (flonum? y))
         (flonum->flonum-not-rational (flmin x y))]
        [(flonum? x)  (cond [(= x +inf.0)  y]
                            [(= x -inf.0)  x]
                            [else  +nan.0])]
        [(flonum? y)  (cond [(= y +inf.0)  x]
                            [(= y -inf.0)  y]
                            [else  +nan.0])]
        [else  (min x y)]))

(: ext-max (case-> (Extended-Integer Extended-Integer -> Extended-Integer)
                   (Extended-Rational Extended-Rational -> Extended-Rational)))
(define (ext-max x y)
  (cond [(and (flonum? x) (flonum? y))
         (flonum->flonum-not-rational (flmax x y))]
        [(flonum? x)  (cond [(= x +inf.0)  x]
                            [(= x -inf.0)  y]
                            [else  +nan.0])]
        [(flonum? y)  (cond [(= y +inf.0)  y]
                            [(= y -inf.0)  x]
                            [else  +nan.0])]
        [else  (max x y)]))

(define-syntax-rule (define-extended-comparison-op name op flop)
  (begin
    (: name (Extended-Rational Extended-Rational -> Boolean))
    (define name
      (make-extended-binary-op
       op flop
       (λ (x) (if (flonum? x) x 0.0))
       (λ (x) x)))))

(define-extended-comparison-op ext= = fl=)
(define-extended-comparison-op ext< < fl<)
(define-extended-comparison-op ext> > fl>)
(define-extended-comparison-op ext<= <= fl<=)
(define-extended-comparison-op ext>= >= fl>=)
