#lang typed/racket/base

(require racket/performance-hint
         "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-polyfun.rkt"
         "../unsafe.rkt")

(provide flexpm1 flexpsqr flgauss flexp1p flexp2)

(define expm1-poly-numer
  (make-flpolyfun
   (-0.28127670288085937e-1
     0.51278186299064534e0
    -0.6310029069350198e-1
     0.11638457975729296e-1
    -0.52143390687521003e-3
     0.21491399776965688e-4)))

(define expm1-poly-denom
  (make-flpolyfun
   ( 1.0
    -0.45442309511354755e0
     0.90850389570911714e-1
    -0.10088963629815502e-1
     0.63003407478692265e-3
    -0.17976570003654402e-4)))

(: flexpm1/poly (Float -> Float))
(define (flexpm1/poly x)
  ;; Define negative in terms of positive to avoid cancellation error
  (cond [(x . fl< . 0.0)  (define y (flexpm1/poly (- x)))
                          (fl/ (- y) (fl+ y 1.0))]
        [else  (fl+ (fl* x 0.10281276702880859e1)
                    (fl* x (fl/ (expm1-poly-numer x) (expm1-poly-denom x))))]))

;; Integer arguments for flexp2
(: flexp2s (Vectorof Nonnegative-Flonum))
(define flexp2s (build-vector (- 1024 -1074) (λ: ([n : Index]) (fl (expt 2 (- n 1074))))))

(begin-encourage-inline
  
  (: flexpm1 (Float -> Float))
  ;; Computes exp(x)-1 in a way that is accurate for small x
  (define (flexpm1 x)
    (define ax (flabs x))
    (cond [(ax . fl>= . 0.5)  (fl- (flexp x) 1.0)]
          [(ax . fl> . (fl* 0.5 epsilon.0))  (flexpm1/poly x)]
          [else  x]))
  
  (: flgauss (Flonum -> Flonum))
  ;; Computes exp(-x^2) in a way that is accurate for large x
  (define (flgauss x)
    (let ([x  (flabs x)])
      (cond [(x . fl> . 28.0)  0.0]
            [(x . fl> . 1.0)
             ;; Split x into a flonum with 21 high-order fractional bits and a part with the rest
             ;; Sometime after p > 26.1, (exp (- (* p p))) outputs subnormals, so we don't go there
             (define p (flmin 26.0 (fl/ (fltruncate (fl* (flexpt 2.0 21.0) x)) (flexpt 2.0 21.0))))
             (define q (fl- x p))
             (fl* (fl* (flexp (- (fl* 2.0 (fl* p q))))
                       (flexp (- (fl* q q))))
                  (flexp (- (fl* p p))))]
            [else
             (flexp (- (fl* x x)))])))
  
  (: flexpsqr (Flonum -> Flonum))
  ;; Computes exp(x^2) in a way that is accurate for large x
  (define (flexpsqr x)
    (let ([x  (flabs x)])
      (cond [(x . fl> . 27.0)  +inf.0]
            [(x . fl> . 1.0)
             (define p (fl/ (fltruncate (fl* (flexpt 2.0 21.0) x)) (flexpt 2.0 21.0)))
             (define q (fl- x p))
             (fl* (fl* (flexp (fl* 2.0 (fl* p q)))
                       (flexp (fl* q q)))
                  (flexp (fl* p p)))]
            [else
             (flexp (fl* x x))])))
  
  (: flexp1p (Flonum -> Flonum))
  ;; Computes exp(1+x) in a way that is accurate near powers of 2
  (define (flexp1p x)
    (cond [(x . fl< . -0.5)  (flexp (fl+ 1.0 x))]
          [else
           (define lg2x (flfloor (fl/ (fllog x) (fllog 2.0))))
           (define lg2x+1 (flfloor (fl/ (fllog (fl+ 1.0 x)) (fllog 2.0))))
           (cond [(fl= lg2x lg2x+1)  (flexp (fl+ 1.0 x))]
                 [else  (fl* (flexp x) (flexp 1.0))])]))
  
  (: flexp2 (Flonum -> Nonnegative-Flonum))
  (define (flexp2 x)
    (cond [(fl<= x -1075.0)  0.0]
          [(fl>= x 1024.0)  +inf.0]
          [(fl= x (flround x))  (unsafe-vector-ref flexp2s (fl->exact-integer (fl+ x 1074.0)))]
          [else  (flexpt 2.0 x)]))
  
  )  ; begin-encourage-inline
