#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../unsafe.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flexponential-pdf
         flexponential-cdf
         flexponential-inv-cdf
         flexponential-sample
         Exponential-Dist exponential-dist exponential-dist-mean)

(: flexponential-pdf (Float Float Any -> Float))
(define flexponential-pdf
  (make-one-sided-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (if log? (- x) (flexp (- x))))))

(: flexponential-cdf (Float Float Any Any -> Float))
(define flexponential-cdf
  (make-one-sided-scale-flcdf
   (λ: ([x : Float] [log? : Any] [1-p? : Any])
     (cond [1-p?  (if log? (- x) (flexp (- x)))]
           [else  (if log? (lg1- (- x)) (- (flexpm1 (- x))))]))))

(: standard-flexponential-inv-cdf (Float Any Any -> Float))
(define (standard-flexponential-inv-cdf q log? 1-p?)
  (cond [1-p?  (if log? (- q) (- (fllog q)))]
        [else  (if log? (- (lg1- q)) (- (fllog1p (- q))))]))

(: flexponential-inv-cdf (Float Float Any Any -> Float))
(define flexponential-inv-cdf (make-one-sided-scale-flinv-cdf standard-flexponential-inv-cdf))

(: flexponential-sample (Flonum Integer -> FlVector))
(define (flexponential-sample s n)
  (cond [(n . < . 0)  (raise-argument-error 'flexponential-sample "Natural" 1 s n)]
        [else  (build-flvector n (λ (_) (fl* s (- (fllog (random))))))]))

;; ===================================================================================================
;; Distribution object

(define-real-dist: exponential-dist Exponential-Dist
  exponential-dist-struct ([mean : Flonum]))

(begin-encourage-inline
  
  (: exponential-dist (case-> (-> Exponential-Dist)
                              (Real -> Exponential-Dist)))
  (define (exponential-dist [s 1.0])
    (let ([s  (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flexponential-pdf s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                    (flexponential-cdf s (fl x) log? 1-p?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                        (flexponential-inv-cdf s (fl p) log? 1-p?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flexponential-sample s 1) 0)]
                       [([n : Integer])  (flvector->list (flexponential-sample s n))]))
      (exponential-dist-struct pdf sample cdf inv-cdf 0.0 +inf.0 (delay (fl* s (fllog 2.0))) s)))
  
  )
