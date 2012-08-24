#lang typed/racket/base

(require racket/flonum
         racket/performance-hint)

(provide Real-Density-Function
         Real-Distribution-Function
         Float-Not-Rational flonum-not-rational?
         Extended-Integer extended-integer?
         flonum->extended-integer
         Integer-Density-Function
         Integer-Distribution-Function
         Integer-Inverse-Distribution-Function
         flprobability? probability?)

(define-type Real-Density-Function
  (case-> (Real -> Float)
          (Real Any -> Float)))

(define-type Real-Distribution-Function
  (case-> (Real -> Float)
          (Real Any -> Float)
          (Real Any Any -> Float)))

(define-type Float-Not-Rational (U +inf.0 -inf.0 Float-Nan))
(define-type Extended-Integer (U Integer Float-Not-Rational))

(define-predicate flonum-not-rational? Float-Not-Rational)
(define-predicate extended-integer? Extended-Integer)

(: flonum->extended-integer (Float -> Extended-Integer))
(define (flonum->extended-integer k)
  (cond [(integer? k)  (fl->exact-integer k)]
        [(flonum-not-rational? k)  k]
        [else  +nan.0]))

(define-type Integer-Density-Function
  (case-> (Extended-Integer -> Float)
          (Extended-Integer Any -> Float)))

(define-type Integer-Distribution-Function
  (case-> (Extended-Integer -> Float)
          (Extended-Integer Any -> Float)
          (Extended-Integer Any Any -> Float)))

(define-type Integer-Inverse-Distribution-Function
  (case-> (Real -> Extended-Integer)
          (Real Any -> Extended-Integer)
          (Real Any Any -> Extended-Integer)))

(begin-encourage-inline
  
  (: flprobability? (Float Any -> Boolean))
  (define (flprobability? p log?)
    (cond [log?  (and (p . >= . -inf.0) (p . <= . 0.0))]
          [else  (and (p . >= . 0.0) (p . <= . 1.0))]))
  
  (: probability? (case-> (Real -> Boolean)
                          (Real Any -> Boolean)))
  (define (probability? p [log? #f])
    (flprobability? (real->double-flonum p) log?))
  
  )  ; begin-encourage-inline
