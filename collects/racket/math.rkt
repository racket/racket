;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; math.rkt: some extra math routines
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

#lang racket/base

(require "unsafe/ops.rkt"
         "performance-hint.rkt")

(provide pi pi.f
         nan? infinite?
         sqr
         sgn conjugate
         sinh cosh tanh
         degrees->radians radians->degrees
         exact-round exact-floor exact-ceiling exact-truncate
         order-of-magnitude)

(define pi   (atan 0 -1))
(define pi.f (atan 0.0f0 -1.0f0))

(begin-encourage-inline

  ;; real predicates
  (define (nan? x)
    (unless (real? x) (raise-argument-error 'nan? "real?" x))
    (or (eqv? x +nan.0) (eqv? x +nan.f)))

  (define (infinite? x)
    (unless (real? x) (raise-argument-error 'infinite? "real?" x))
    (or (= x +inf.0) (= x -inf.0)))

  ;; z^2
  (define (sqr z)
    (unless (number? z) (raise-argument-error 'sqr "number?" z))
    (* z z))

  ;; sgn function
  (define (sgn x)
    (unless (real? x) (raise-argument-error 'sgn "real?" x))
    (cond [(= 0 x) x]  ; preserve 0, 0.0 and 0.0f0
          [(double-flonum? x) (cond [(unsafe-fl> x 0.0)  1.0]
                                    [(unsafe-fl< x 0.0) -1.0]
                                    [else               +nan.0])]
          [(single-flonum? x) (cond [(> x 0.0f0)  1.0f0]
                                    [(< x 0.0f0) -1.0f0]
                                    [else        +nan.f])]
          [else               (if (> x 0) 1 -1)]))

  ;; complex conjugate
  (define (conjugate z)
    (unless (number? z) (raise-argument-error 'conjugate "number?" z))
    (make-rectangular (real-part z) (- (imag-part z))))

  ;; complex hyperbolic functions
  (define (sinh z)
    (unless (number? z) (raise-argument-error 'sinh "number?" z))
    (cond [(= z 0) z]    ; preserve 0, 0.0, -0.0, 0.0f0, 0.0+0.0i, etc.
          [(real? z)
           (let loop ([z z])
             (cond [(z . < . 0) (- (loop (- z)))]
                   [else        (/ (- (exp z) (exp (- z))) 2)]))]
          [else (/ (- (exp z) (exp (- z))) 2)]))

  (define (cosh z)
    (unless (number? z) (raise-argument-error 'cosh "number?" z))
    (cond [(and (real? z) (= z 0)) (if (single-flonum? z) 1.0f0 1.0)]
          [else                    (/ (+ (exp z) (exp (- z))) 2)]))

  (define (tanh z)
    (unless (number? z) (raise-argument-error 'tanh "number?" z))
    (cond [(= z 0) z]  ; preserve 0, 0.0, -0.0, 0.0f0, 0.0+0.0i, etc.
          [(real? z)
           (let loop ([z z])
             (cond [(z . < . 0)   (- (loop (- z)))]
                   [(z . < . 20)  (define exp2z (exp (* 2 z)))
                                  (/ (- exp2z 1) (+ exp2z 1))]
                   [(z . >= . 20) (if (single-flonum? z) 1.0f0 1.0)]
                   [else          z]))]  ; +nan.0 or +nan.f
          [else
           (define exp2z (exp (* 2 z)))
           (/ (- exp2z 1) (+ exp2z 1))]))

  ;; angle conversion
  (define (degrees->radians x)
    (unless (real? x) (raise-argument-error 'degrees->radians "real?" x))
    (cond [(single-flonum? x) (* x (/ pi.f 180f0))]
          [else               (* x (/ pi 180.0))]))

  (define (radians->degrees x)
    (unless (real? x) (raise-argument-error 'radians->degrees "real?" x))
    (cond [(single-flonum? x) (* x (/ 180f0 pi.f))]
          [else               (* x (/ 180.0 pi))]))

  ;; inexact->exact composed with round, floor, ceiling, truncate
  (define-syntax-rule (define-integer-conversion name convert)
    (define (name x)
      (unless (rational? x) (raise-argument-error 'name "rational?" x))
      (inexact->exact (convert x))))

  (define-integer-conversion exact-round round)
  (define-integer-conversion exact-floor floor)
  (define-integer-conversion exact-ceiling ceiling)
  (define-integer-conversion exact-truncate truncate)

  )

(define order-of-magnitude
  (let* ([exact-log (λ (x) (inexact->exact (log x)))]
         [inverse-exact-log10 (/ (exact-log 10))])
    (λ (r)
      (unless (and (real? r) (positive? r) (not (= r +inf.0)))
        (raise-argument-error 'order-of-magnitude "(and/c (>/c 0.0) (not/c +inf.0))" r))
      (define q (inexact->exact r))
      (define m
        (floor (* (- (exact-log (numerator q)) (exact-log (denominator q)))
                  inverse-exact-log10)))
      (let loop ([m m] [p (expt 10 m)])
        (if (< q p)
            (loop (sub1 m) (* p 1/10))
            (let ([u (* p 10)])
              (if (>= q u) (loop (add1 m) u) m)))))))

#|
;; Timing tests below provided by Jos Koot for the order-of-magnitude function

#lang racket

;;; Tests and timings of order-of-magnitude

(require "order-of-magnitude.rkt")
(require (planet joskoot/planet-fmt:1:1/fmt))

(define-syntax timer
  (syntax-rules ()
    ((_ type iter k expr)
     (let* ([output-string (open-output-string)]
            [result expr]
            [dummy (parameterize ([current-output-port output-string])
                     (time (for ([k (in-range iter)]) expr)))]
            [input-string (open-input-string (get-output-string output-string))])
       (parameterize ([current-input-port input-string])
         (let ([cpu (begin (read) (read) (read))]
               [real (begin (read) (read) (read))]
               [gc (begin (read) (read) (read))]
               [micro (/ iter 1000)])
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

(let* ([max-expt 10000] [small (expt 10 (- (* 2 max-expt)))] [iter 1000])
  (for ([k (in-range (- max-expt) (add1 max-expt) (/ max-expt 10))])
    (let* ([q (expt 10 k)] [qq (- q small)] [qqq (+ q small)])
      (unless (= k (timer "exact power of 10" iter k (order-of-magnitude q)))
        (error 'test-1 "~s" k))
      (unless (= (sub1 k)
                 (timer "slightly less than power of 10"
                        iter k (order-of-magnitude qq)))
        (error 'test-2 "~s" k))
      (unless (= k
                 (timer "slightly more than power of 10"
                        iter k (order-of-magnitude qqq)))
        (error 'test-3 "~s" k)))))

|#
