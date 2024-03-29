(load-relative "loadtest.rktl")
(Section 'math)
(require racket/math
         racket/flonum
         racket/unsafe/ops)

(define (flonum->bit-field x)
  (cond [(flonum? x)  (integer-bytes->integer (real->floating-point-bytes x 8) #f)]
        [else  (raise-type-error 'flonum->bit-field "flonum" x)]))

(define (bit-field->flonum i)
  (cond [(and (exact-integer? i) (i . >= . 0) (i . <= . #xffffffffffffffff))
         (floating-point-bytes->real (integer->integer-bytes i 8 #f))]
        [else
         (raise-type-error 'bit-field->flonum "exact integer in [0,#xffffffffffffffff]" i)]))

(define (flonum->ordinal x)
  (cond [(flonum? x)  (cond [(x . < . 0)  (- (flonum->bit-field (- x)))]
                            [else            (flonum->bit-field (unsafe-flabs x))])] ; abs for -0.0
        [else  (raise-type-error 'flonum->ordinal "flonum" x)]))

(define (ordinal->flonum i)
  (cond [(and (exact-integer? i) (i . >= . #x-7fffffffffffffff) (i . <= . #x7fffffffffffffff))
         (cond [(i . < . 0)  (- (bit-field->flonum (- i)))]
               [else            (bit-field->flonum i)])]
        [else
         (raise-type-error
          'ordinal->flonum "exact integer in [#x-7fffffffffffffff,#xffffffffffffffff]" i)]))

(define +inf-ordinal (flonum->ordinal +inf.0))
(define -inf-ordinal (flonum->ordinal -inf.0))

(define (flstep x n)
  (cond [(not (flonum? x))  (raise-type-error 'flstep "flonum" 0 x n)]
        [(not (exact-integer? n))  (raise-type-error 'flstep "exact integer" 1 x n)]
        [(eqv? x +nan.0)  +nan.0]
        [(and (eqv? x +inf.0) (n . >= . 0))  +inf.0]
        [(and (eqv? x -inf.0) (n . <= . 0))  -inf.0]
        [else  (define i (+ n (flonum->ordinal x)))
               (cond [(i . < . -inf-ordinal)  -inf.0]
                     [(i . > . +inf-ordinal)  +inf.0]
                     [else  (ordinal->flonum i)])]))

(define (flnext x) (flstep x 1))
(define (flprev x) (flstep x -1))

(define -max.0 (flnext -inf.0))
(define -min.0 (flprev 0.0))
(define +min.0 (flnext 0.0))
(define +max.0 (flprev +inf.0))

(define (double=? x y)
  (and (flonum? y)
       (let ([x  (inexact->exact x)]
             [y  (inexact->exact y)])
         ((abs (- x y)) . < . #e1e-10))))

(define (single=? x y)
  (cond
    [(not (single-flonum-available?))
     (double=? x y)]
    [else
     (and (single-flonum? y)
          (let ([x  (inexact->exact x)]
                [y  (inexact->exact y)])
            ((abs (- x y)) . < . #e1e-6)))]))

;; =========================================================================
;; pi

(test #t single=? #e3.141592653589793238462643383279502884197169399 pi.f)
(test #t double=? #e3.141592653589793238462643383279502884197169399 pi)
(when (single-flonum-available?)
  (test pi.f real->single-flonum pi))

;; =========================================================================
;; nan?

(test #f nan? -1)
(test #f nan? 0)
(test #f nan? 1)

(test #t nan? +nan.f)
(test #f nan? -inf.f)
(test #f nan? -1.0f0)
(test #f nan? -0.0f0)
(test #f nan? 0.0f0)
(test #f nan? 1.0f0)
(test #f nan? +inf.f)

(test #t nan? +nan.0)
(test #f nan? -inf.0)
(test #f nan? -max.0)
(test #f nan? -1.0)
(test #f nan? -min.0)
(test #f nan? -0.0)
(test #f nan? 0.0)
(test #f nan? +min.0)
(test #f nan? 1.0)
(test #f nan? +max.0)
(test #f nan? +inf.0)

;; =========================================================================
;; infinite?

(test #f infinite? -1)
(test #f infinite? 0)
(test #f infinite? 1)

(test #f infinite? +nan.f)
(test #t infinite? -inf.f)
(test #f infinite? -1.0f0)
(test #f infinite? -0.0f0)
(test #f infinite? 0.0f0)
(test #f infinite? 1.0f0)
(test #t infinite? +inf.f)

(test #f infinite? +nan.0)
(test #t infinite? -inf.0)
(test #f infinite? -max.0)
(test #f infinite? -1.0)
(test #f infinite? -min.0)
(test #f infinite? -0.0)
(test #f infinite? 0.0)
(test #f infinite? +min.0)
(test #f infinite? 1.0)
(test #f infinite? +max.0)
(test #t infinite? +inf.0)

;; =========================================================================
;; positive-integer?

(test #t positive-integer? 1)
(test #t positive-integer? 1.0)
(test #t positive-integer? 3/3)
(test #f positive-integer? 3/2)
(test #f positive-integer? 0)
(test #f positive-integer? 0.0)
(test #f positive-integer? -1)
(test #f positive-integer? 1.3)

;; =========================================================================
;; negative-integer?

(test #t negative-integer? -1)
(test #t negative-integer? -1.0)
(test #t negative-integer? -3/3)
(test #f negative-integer? -3/2)
(test #f negative-integer? 0)
(test #f negative-integer? -0)
(test #f negative-integer? 1)
(test #f negative-integer? -1.3)

;; =========================================================================
;; nonpositive-integer?

(test #t nonpositive-integer? -1)
(test #t nonpositive-integer? -1.0)
(test #t nonpositive-integer? -3/3)
(test #t nonpositive-integer? 0)
(test #t nonpositive-integer? -0)
(test #t nonpositive-integer? 0.0)
(test #t nonpositive-integer? -0.0)
(test #f nonpositive-integer? -3/2)
(test #f nonpositive-integer? 1)
(test #f nonpositive-integer? -1.3)

;; =========================================================================
;; nonnegative-integer? 

(test #t nonnegative-integer? 1)
(test #t nonnegative-integer? 1.0)
(test #t nonnegative-integer? 3/3)
(test #t nonnegative-integer? 0)
(test #t nonnegative-integer? -0)
(test #t nonnegative-integer? 0.0)
(test #t nonnegative-integer? -0.0)
(test #f nonnegative-integer? 3/2)
(test #f nonnegative-integer? 0.5)
(test #f nonnegative-integer? -5)

;; =========================================================================
;; natural?

(test #t natural? 1)
(test #t natural? 3/3)
(test #t natural? 0)
(test #t natural? -0)
(test #f natural? 1.0)
(test #f natural? 0.0)
(test #f natural? -0.0)
(test #f natural? 3/2)
(test #f natural? 0.5)
(test #f natural? -5)


;; =========================================================================
;; sqr

(test 4 sqr -2)
(test 1 sqr -1)
(test 0 sqr 0)
(test 1 sqr 1)
(test 4 sqr 2)

(test +nan.f sqr +nan.f)
(test +inf.f sqr -inf.f)
(test 4.0f0 sqr -2.0f0)
(test 1.0f0 sqr -1.0f0)
(test 0.0f0 sqr -0.0f0)
(test 0.0f0 sqr 0.0f0)
(test 1.0f0 sqr 1.0f0)
(test 4.0f0 sqr 2.0f0)
(test +inf.f sqr +inf.f)

(test +nan.0 sqr +nan.0)
(test +inf.0 sqr -inf.0)
(test +inf.0 sqr -max.0)
(test 4.0 sqr -2.0)
(test 1.0 sqr -1.0)
(test 0.0 sqr -min.0)
(test 0.0 sqr -0.0)
(test 0.0 sqr 0.0)
(test 0.0 sqr +min.0)
(test 1.0 sqr 1.0)
(test 4.0 sqr 2.0)
(test +inf.0 sqr +max.0)
(test +inf.0 sqr +inf.0)

;; =========================================================================
;; sgn

(test -1 sgn -2)
(test -1 sgn -1)
(test 0 sgn 0)
(test 1 sgn 1)
(test 1 sgn 2)

(test +nan.f sgn +nan.f)
(test -1.0f0 sgn -inf.f)
(test -1.0f0 sgn -1.0f0)
(test -0.0f0 sgn -0.0f0)
(test 0.0f0 sgn 0.0f0)
(test 1.0f0 sgn 1.0f0)
(test 1.0f0 sgn +inf.f)

(test +nan.0 sgn +nan.0)
(test -1.0 sgn -inf.0)
(test -1.0 sgn -max.0)
(test -1.0 sgn -1.0)
(test -1.0 sgn -min.0)
(test -0.0 sgn -0.0)
(test 0.0 sgn 0.0)
(test 1.0 sgn +min.0)
(test 1.0 sgn 1.0)
(test 1.0 sgn +max.0)
(test 1.0 sgn +inf.0)

;; =========================================================================
;; sinh

(define sinh+1 1.1752011936438014568823818505956008151557179813341)
(define sinh-1 (- sinh+1))

(test #t double=? sinh-1 (sinh -1))
(test 0 sinh 0)
(test #t double=? sinh+1 (sinh 1))

#reader "maybe-single.rkt"
(begin
  (test +nan.f sinh +nan.f)
  (test -inf.f sinh -inf.f)
  (test #t single=? sinh-1 (sinh -1.0f0))
  (test 0.0f0 sinh 0.0f0)
  (test #t single=? sinh+1 (sinh 1.0f0))
  (test +inf.f sinh +inf.f))

(test +nan.0 sinh +nan.0)
(test -inf.0 sinh -inf.0)
(test -inf.0 sinh -max.0)
(test #t double=? sinh-1 (sinh -1.0))
(test -min.0 sinh -min.0)
(test -0.0 sinh -0.0)
(test 0.0 sinh 0.0)
(test +min.0 sinh +min.0)
(test #t double=? sinh+1 (sinh 1.0))
(test +inf.0 sinh +max.0)
(test +inf.0 sinh +inf.0)

;; =========================================================================
;; cosh

(define cosh+1 #e1.5430806348152437784779056207570616826015291123659)

(test #t double=? cosh+1 (cosh -1))
(test 1.0 cosh 0)
(test #t double=? cosh+1 (cosh 1))

#reader "maybe-single.rkt"
(begin
  (test +nan.f cosh +nan.f)
  (test +inf.f cosh -inf.f)
  (test #t single=? cosh+1 (cosh -1.0f0))
  (test 1.0f0 cosh -0.0f0)
  (test 1.0f0 cosh 0.0f0)
  (test #t single=? cosh+1 (cosh 1.0f0))
  (test +inf.f cosh +inf.f))

(test +nan.0 cosh +nan.0)
(test +inf.0 cosh -inf.0)
(test +inf.0 cosh -max.0)
(test #t double=? cosh+1 (cosh -1.0))
(test 1.0 cosh -min.0)
(test 1.0 cosh -0.0)
(test 1.0 cosh 0.0)
(test 1.0 cosh +min.0)
(test #t double=? cosh+1 (cosh 1.0))
(test +inf.0 cosh +max.0)
(test +inf.0 cosh +inf.0)

;; =========================================================================
;; tanh

(define tanh+1 #e0.76159415595576488811945828260479359041276859725794)
(define tanh-1 (- tanh+1))

(test -1.0 tanh -20)
(test #t double=? tanh-1 (tanh -1))
(test 0 tanh 0)
(test #t double=? tanh+1 (tanh 1))
(test 1.0 tanh 20)
(test #t flonum? (tanh (inexact->exact 1/100000000000)))

#reader "maybe-single.rkt"
(begin
  (test +nan.f tanh +nan.f)
  (test -1.0f0 tanh -inf.f)
  (test -1.0f0 tanh -20.0f0)
  (test #t single=? tanh-1 (tanh -1.0f0))
  (test -0.0f0 tanh -0.0f0)
  (test 0.0f0 tanh 0.0f0)
  (test #t single=? tanh+1 (tanh 1.0f0))
  (test 1.0f0 tanh 20.0f0)
  (test 1.0f0 tanh +inf.f)
  (when (single-flonum-available?)
    (test #t single-flonum? (tanh -0.11100646f0))
    (test #t single-flonum? (tanh 0.11100646f0))
    (test #t single-flonum? (tanh 0.61100646f0))
    (test #t single-flonum? (tanh 20.61100646f0))))

(test +nan.0 tanh +nan.0)
(test -1.0 tanh -inf.0)
(test -1.0 tanh -max.0)
(test -1.0 tanh -20.0)
(test #t double=? tanh-1 (tanh -1.0))
(test -min.0 tanh -min.0)
(test -0.0 tanh -0.0)
(test 0.0 tanh 0.0)
(test +min.0 tanh +min.0)
(test #t double=? tanh+1 (tanh 1.0))
(test 1.0 tanh 20.0)
(test 1.0 tanh +max.0)
(test 1.0 tanh +inf.0)

;; =========================================================================
;; degrees->radians

(test #t double=? (- pi) (degrees->radians -180))
(test #t double=? (* -1/2 pi) (degrees->radians -90))
(test 0 degrees->radians 0)
(test #t double=? (* 1/2 pi) (degrees->radians 90))
(test #t double=? pi (degrees->radians 180))

#reader "maybe-single.rkt"
(begin
  (test +nan.f degrees->radians +nan.f)
  (test -inf.f degrees->radians -inf.f)
  (test #t single=? (- pi) (degrees->radians -180.0f0))
  (test #t single=? (* -1/2 pi) (degrees->radians -90.0f0))
  (test -0.0f0 degrees->radians -0.0f0)
  (test 0.0f0 degrees->radians 0.0f0)
  (test #t single=? (* 1/2 pi) (degrees->radians 90.0f0))
  (test #t single=? pi (degrees->radians 180.0f0))
  (test +inf.f degrees->radians +inf.f))

(test +nan.0 degrees->radians +nan.0)
(test -inf.0 degrees->radians -inf.0)
(test #t double=? (- pi) (degrees->radians -180.0))
(test #t double=? (* -1/2 pi) (degrees->radians -90.0))
(test -0.0 degrees->radians -min.0)
(test -0.0 degrees->radians -0.0)
(test 0.0 degrees->radians 0.0)
(test 0.0 degrees->radians +min.0)
(test #t double=? (* 1/2 pi) (degrees->radians 90.0))
(test #t double=? pi (degrees->radians 180.0))
(test +inf.0 degrees->radians +inf.0)

;; =========================================================================
;; radians->degrees

(test 0 radians->degrees 0)

#reader "maybe-single.rkt"
(begin
  (test +nan.f radians->degrees +nan.f)
  (test -inf.f radians->degrees -inf.f)
  (test #t single=? -180 (radians->degrees (- pi.f)))
  (test #t single=? -90 (radians->degrees (* -1/2 pi.f)))
  (test -0.0f0 radians->degrees -0.0f0)
  (test 0.0f0 radians->degrees 0.0f0)
  (test #t single=? 90 (radians->degrees (* 1/2 pi.f)))
  (test #t single=? 180 (radians->degrees pi.f))
  (test +inf.f radians->degrees +inf.f))

(test +nan.0 radians->degrees +nan.0)
(test -inf.0 radians->degrees -inf.0)
(test -inf.0 radians->degrees -max.0)
(test #t double=? -180 (radians->degrees (- pi)))
(test #t double=? -90 (radians->degrees (* -1/2 pi)))
(test -0.0 radians->degrees -0.0)
(test 0.0 radians->degrees 0.0)
(test #t double=? 90 (radians->degrees (* 1/2 pi)))
(test #t double=? 180 (radians->degrees pi))
(test +inf.0 radians->degrees +max.0)
(test +inf.0 radians->degrees +inf.0)

;; =========================================================================
;; exact-round

(test -2 exact-round #e-1.5)
(test 0 exact-round #e-0.5)
(test 0 exact-round #e0.5)
(test 2 exact-round #e1.5)

#reader "maybe-single.rkt"
(begin
  (err/rt-test (exact-round +nan.f))
  (err/rt-test (exact-round -inf.f))
  (test -2 exact-round -1.5f0)
  (test 0 exact-round -0.5f0)
  (test 0 exact-round 0.5f0)
  (test 2 exact-round 1.5f0)
  (err/rt-test (exact-round +inf.f)))

(err/rt-test (exact-round +nan.0))
(err/rt-test (exact-round -inf.0))
(test (inexact->exact -max.0) exact-round -max.0)
(test -2 exact-round -1.5)
(test 0 exact-round -0.5)
(test 0 exact-round -min.0)
(test 0 exact-round +min.0)
(test 0 exact-round 0.5)
(test 2 exact-round 1.5)
(test (inexact->exact +max.0) exact-round +max.0)
(err/rt-test (exact-round +inf.0))

;; =========================================================================
;; exact-floor

(test -2 exact-floor #e-1.5)
(test -1 exact-floor #e-0.5)
(test 0 exact-floor #e0.5)
(test 1 exact-floor #e1.5)

#reader "maybe-single.rkt"
(begin
  (err/rt-test (exact-floor +nan.f))
  (err/rt-test (exact-floor -inf.f))
  (test -2 exact-floor -1.5f0)
  (test -1 exact-floor -0.5f0)
  (test 0 exact-floor 0.5f0)
  (test 1 exact-floor 1.5f0)
  (err/rt-test (exact-floor +inf.f)))

(err/rt-test (exact-floor +nan.0))
(err/rt-test (exact-floor -inf.0))
(test (inexact->exact -max.0) exact-floor -max.0)
(test -2 exact-floor -1.5)
(test -1 exact-floor -0.5)
(test -1 exact-floor -min.0)
(test 0 exact-floor +min.0)
(test 0 exact-floor 0.5)
(test 1 exact-floor 1.5)
(test (inexact->exact +max.0) exact-floor +max.0)
(err/rt-test (exact-floor +inf.0))

;; =========================================================================
;; exact-ceiling

(test -1 exact-ceiling #e-1.5)
(test 0 exact-ceiling #e-0.5)
(test 1 exact-ceiling #e0.5)
(test 2 exact-ceiling #e1.5)

#reader "maybe-single.rkt"
(begin
  (err/rt-test (exact-ceiling +nan.f))
  (err/rt-test (exact-ceiling -inf.f))
  (test -1 exact-ceiling -1.5f0)
  (test 0 exact-ceiling -0.5f0)
  (test 1 exact-ceiling 0.5f0)
  (test 2 exact-ceiling 1.5f0)
  (err/rt-test (exact-ceiling +inf.f)))

(err/rt-test (exact-ceiling +nan.0))
(err/rt-test (exact-ceiling -inf.0))
(test (inexact->exact -max.0) exact-ceiling -max.0)
(test -1 exact-ceiling -1.5)
(test 0 exact-ceiling -0.5)
(test 0 exact-ceiling -min.0)
(test 1 exact-ceiling +min.0)
(test 1 exact-ceiling 0.5)
(test 2 exact-ceiling 1.5)
(test (inexact->exact +max.0) exact-ceiling +max.0)
(err/rt-test (exact-ceiling +inf.0))

;; =========================================================================
;; exact-truncate

(test -1 exact-truncate #e-1.5)
(test 0 exact-truncate #e-0.5)
(test 0 exact-truncate #e0.5)
(test 1 exact-truncate #e1.5)

#reader "maybe-single.rkt"
(begin
  (err/rt-test (exact-truncate +nan.f))
  (err/rt-test (exact-truncate -inf.f))
  (test -1 exact-truncate -1.5f0)
  (test 0 exact-truncate -0.5f0)
  (test 0 exact-truncate 0.5f0)
  (test 1 exact-truncate 1.5f0)
  (err/rt-test (exact-truncate +inf.f)))

(err/rt-test (exact-truncate +nan.0))
(err/rt-test (exact-truncate -inf.0))
(test (inexact->exact -max.0) exact-truncate -max.0)
(test -1 exact-truncate -1.5)
(test 0 exact-truncate -0.5)
(test 0 exact-truncate -min.0)
(test 0 exact-truncate +min.0)
(test 0 exact-truncate 0.5)
(test 1 exact-truncate 1.5)
(test (inexact->exact +max.0) exact-truncate +max.0)
(err/rt-test (exact-truncate +inf.0))

;; =========================================================================
;; comparison with conversion to exact BEFORE integer conversion, in the
;; range near where floating-point numbers become integer-only

(define (test-integer-conversion convert exact-convert)
  (for* ([y  '(-0.75 -0.5 -0.25 0.0 0.25 0.5 0.75)]
         [e  (in-range 50 54)]
         [i  (in-range -1 2)])
    (define x (+ y (ordinal->flonum (+ i (flonum->ordinal (expt 2.0 e))))))
    (test (convert (inexact->exact x)) exact-convert x)))

(test-integer-conversion round exact-round)
(test-integer-conversion floor exact-floor)
(test-integer-conversion ceiling exact-ceiling)
(test-integer-conversion truncate exact-truncate)

;; =========================================================================
;; order-of-magnitude

(test 0 order-of-magnitude 1)
(test 0 order-of-magnitude 9)
(test 1 order-of-magnitude 10)
(test 1 order-of-magnitude 17)
(test 1 order-of-magnitude 99)
(test 2 order-of-magnitude 100)
(test 2 order-of-magnitude 200)
(test 2 order-of-magnitude 999)
(test 3 order-of-magnitude 1000)
(test 3 order-of-magnitude 5000)
(test 3 order-of-magnitude 9999)
(test 4 order-of-magnitude 10000)
(test -2 order-of-magnitude 1/100)
(test -3 order-of-magnitude 1/101)

;; =========================================================================

(report-errs)
