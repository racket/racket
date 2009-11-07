;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; math.ss: some extra math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang scheme/base
(provide pi
         sqr
         sgn conjugate
         sinh cosh tanh
         order-of-magnitude)

(define (sqr z) (* z z))

(define pi (atan 0 -1))

;; sgn function
(define (sgn x)
  (if (exact? x)
    (cond [(< x 0) -1] [(> x 0) 1] [else 0])
    (cond [(< x 0.0) -1.0] [(> x 0.0) 1.0] [else 0.0])))

;; complex conjugate
(define (conjugate z)
  (make-rectangular (real-part z) (- (imag-part z))))

;; real hyperbolic functions
(define (sinh x)
  (/ (- (exp x) (exp (- x))) 2.0))

(define (cosh x)
  (/ (+ (exp x) (exp (- x))) 2.0))

(define (tanh x) (/ (sinh x) (cosh x)))

(define order-of-magnitude
  (let* ([exact-log (λ (x) (inexact->exact (log x)))]
         [inverse-exact-log10 (/ (exact-log 10))])
    (λ (r)
      (unless (and (real? r) (positive? r))
        (raise-type-error 'order-of-magnitude "positive real number" r))
      (let* ([q (inexact->exact r)]
             [m
              (floor
               (* (- (exact-log (numerator q)) (exact-log (denominator q)))
                  inverse-exact-log10))])
        (let loop ((m m) (p (expt 10 m)))
          (if (< q p) (loop (sub1 m) (* p 1/10))
              (let ((u (* p 10)))
                (if (>= q u) (loop (add1 m) u) m))))))))

#|
;; Timing tests below provided by Jos Koot for the order-of-magnitude function

#lang scheme

;;; Tests and timings of order-of-magnitude

(require "order-of-magnitude.ss")
(require (planet joskoot/planet-fmt:1:1/fmt))

(define-syntax timer
 (syntax-rules ()
  ((_ type iter k expr)
   (let*
    ((output-string (open-output-string))
     (result expr)
    (dummy
     (parameterize ((current-output-port output-string))
      (time (for ((k (in-range iter))) expr))))
    (input-string (open-input-string (get-output-string output-string))))
    (parameterize ((current-input-port input-string))
     (let
      ((cpu (begin (read) (read) (read)))
       (real (begin (read) (read) (read)))
       (gc (begin (read) (read) (read)))
       (micro (/ iter 1000)))
      (if (and (>= cpu 0) (>= real 0) (>= gc 0))
       ((fmt
         "'test type : ' d/
          'exponent  : ' i6/
          'n-obs     : ' i6/
          'mean cpu  : ' i6 x 'microseconds'/
          'mean real : ' i6 x 'microseconds'/
          'mean gc   : ' i6 x 'microseconds'/
          'real - gc : ' i6 x 'microseconds'//" 'current)
        type
        k
        iter
        (/ cpu micro)
        (/ real micro)
        (/ gc micro)
        (/ (- cpu gc) micro))
       ((fmt "'incorrect times for k='i//" 'current) k))))
    result))))
    

(let* ((max-expt 10000) (small (expt 10 (- (* 2 max-expt)))) (iter 1000))
 (for ((k (in-range (- max-expt) (add1 max-expt) (/ max-expt 10))))
  (let* ((q (expt 10 k)) (qq (- q small)) (qqq (+ q small)))
   (unless
    (= k (timer "exact power of 10" iter k (order-of-magnitude q)))
    (error 'test-1 "~s" k))
   (unless
    (= (sub1 k)
     (timer "slightly less than power of 10" iter k (order-of-magnitude qq)))
    (error 'test-2 "~s" k))
   (unless
    (= k
     (timer "slightly more than power of 10" iter k (order-of-magnitude qqq)))
    (error 'test-3 "~s" k)))))

|#