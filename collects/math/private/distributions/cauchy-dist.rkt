#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         "../../flonum.rkt"
         "../../base.rkt"
         "../unsafe.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide flcauchy-pdf
         flcauchy-cdf
         flcauchy-inv-cdf
         flcauchy-sample
         Cauchy-Dist cauchy-dist cauchy-dist-mode cauchy-dist-scale)

(: flcauchy-pdf (Float Float Float Any -> Float))
(define flcauchy-pdf
  (make-symmetric-location-scale-flpdf
   (λ: ([x : Float] [log? : Any])
     (cond [(x . fl> . 1e100)
            ;; Avoid overflow in (* x x) by using 1/x
            (define 1/x (fl/ 1.0 x))
            (define 1/x^2 (fl* 1/x 1/x))
            (cond [log?  (fl- (fl- (fllog 1/x^2) (fllog1p 1/x^2)) (fllog pi))]
                  [else  (fl/ 1/x^2 (fl* pi (fl+ 1.0 1/x^2)))])]
           [else
            (cond [log?  (fl- (- (fllog1p (fl* x x))) (fllog pi))]
                  [else  (fl/ 1.0 (fl* pi (fl+ 1.0 (fl* x x))))])]))))

(: flcauchy-cdf (Float Float Float Any Any -> Float))
(define flcauchy-cdf
  (make-symmetric-location-scale-flcdf
   (λ: ([x : Float] [log? : Any])
     ;; Preserve precision by making the argument to `flatan' smaller than 1
     (cond [log?  (cond [(x . fl< . -1.0)  (fl- (fllog (flatan (fl/ -1.0 x))) (fllog pi))]
                        [(x . fl> . +1.0)  (fllog1p (fl/ (flatan (fl/ -1.0 x)) pi))]
                        [else  (fllog (fl+ 0.5 (fl/ (flatan x) pi)))])]
           [else  (cond [(x . fl< . -1.0)  (- (fl/ (flatan (fl/ 1.0 x)) pi))]
                        [(x . fl> . +1.0)  (fl- 1.0 (fl/ (flatan (fl/ 1.0 x)) pi))]
                        [else  (fl+ 0.5 (fl/ (flatan x) pi))])]))))

(: standard-flcauchy-inv-cdf (Float Any -> Float))
(define (standard-flcauchy-inv-cdf q log?)
  ;; Preserve precision by getting the argument to `fltan' close to 0.0
  (cond [log?
         (cond [(q . fl> . (fllog 0.75))  (fl/ +1.0 (fltan (fl* pi (- (flexpm1 q)))))]
               [(q . fl< . (fllog 0.25))  (fl/ -1.0 (fltan (fl* pi (flexp q))))]
               [else  (fltan (fl* pi (fl- (flexp q) 0.5)))])]
        [else
         (cond [(q . fl> . 0.75)  (fl/ +1.0 (fltan (fl* pi (fl- 1.0 q))))]
               [(q . fl< . 0.25)  (fl/ -1.0 (fltan (fl* pi q)))]
               [else  (fltan (fl* pi (fl- q 0.5)))])]))

(: flcauchy-inv-cdf (Float Float Float Any Any -> Float))
(define flcauchy-inv-cdf (make-symmetric-location-scale-flinv-cdf standard-flcauchy-inv-cdf))

(: flcauchy-sample-single (Flonum Flonum -> Flonum))
(define flcauchy-sample-single
  (make-symmetric-location-scale-flrandom standard-flcauchy-inv-cdf))

(: flcauchy-sample (Float Float Integer -> FlVector))
(define (flcauchy-sample c s n)
  (cond [(n . < . 0)  (raise-argument-error 'flcauchy-sample "Natural" 2 c s n)]
        [else  (build-flvector n (λ (_) (flcauchy-sample-single c s)))]))

;; ===================================================================================================
;; Distribution object

(define-real-dist: cauchy-dist Cauchy-Dist
  cauchy-dist-struct ([mode : Float] [scale : Float]))

(begin-encourage-inline
  
  (: cauchy-dist (case-> (-> Cauchy-Dist)
                         (Real -> Cauchy-Dist)
                         (Real Real -> Cauchy-Dist)))
  (define (cauchy-dist [c 0.0] [s 1.0])
    (let ([c  (fl c)] [s  (fl s)])
      (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                    (flcauchy-pdf c s (fl x) log?)))
      (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [complement? : Any #f])
                    (flcauchy-cdf c s (fl x) log? complement?)))
      (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [complement? : Any #f])
                        (flcauchy-inv-cdf c s (fl p) log? complement?)))
      (define sample (case-lambda:
                       [()  (unsafe-flvector-ref (flcauchy-sample c s 1) 0)]
                       [([n : Integer])  (flvector->list (flcauchy-sample c s n))]))
      (cauchy-dist-struct pdf sample cdf inv-cdf -inf.0 +inf.0 (delay c) c s)))
  
  )
