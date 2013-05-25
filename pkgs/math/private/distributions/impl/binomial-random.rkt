#lang typed/racket/base

#|
Wolfgang Hormann. The Generation of Binomial Random Variates.
|#

(require "../../../base.rkt"
         "../../../flonum.rkt"
         "../../unsafe.rkt"
         "normal-random.rkt")

(provide flbinomial-sample)

(: flbinomial-sample-small (Flonum Flonum Natural -> FlVector))
;; For n*min(p,1-p) <= 30
(define (flbinomial-sample-small n p m)
  (let-values ([(p q s?)  (cond [(p . fl< . 0.5)  (values p (fl- 1.0 p) #f)]
                                [else             (values (fl- 1.0 p) p #t)])])
    (define q^n (flexpt q n))
    (define r (fl/ p q))
    (define g (fl* r (fl+ n 1.0)))
    (build-flvector
     m (λ (_)
         (define k
           (let: reject : Flonum ()
             (let loop ([k 0.0] [f q^n] [u  (random)])
               (cond [(u . fl< . f)  k]
                     [(k . fl> . 110.0)  (reject)]
                     [else  (let ([k  (fl+ k 1.0)])
                              (loop k (fl* f (fl- (fl/ g k) r)) (fl- u f)))]))))
         (if s? (fl- n k) k)))))

(: flbinomial-sample-hormann (Flonum Flonum Natural -> FlVector))
;; For n*min(p,1-p) >= 10
(define (flbinomial-sample-hormann n p j)
  (let-values ([(p q s?)  (cond [(p . fl< . 0.5)  (values p (fl- 1.0 p) #f)]
                                [else             (values (fl- 1.0 p) p #t)])])
    (define σ (flsqrt (* n p q)))
    (define m (flfloor (fl* (fl+ n 1.0) p)))
    
    (define b (fl+ 1.15 (fl* 2.53 σ)))
    (define a (+ -0.0873 (fl* 0.0248 b) (fl* 0.01 p)))
    (define c (fl+ 0.5 (fl* n p)))
    (define α (fl* σ (fl+ 2.83 (fl/ 5.1 b))))
    (define vr (fl- 0.92 (fl/ 4.2 b)))
    (build-flvector
     j (λ (_)
         (define k
           (let: loop : Flonum ()
             (define v (random))
             (define u (fl- (random) 0.5))
             (define us (fl- 0.5 (flabs u)))
             (define k (flfloor (fl+ c (fl* u (fl+ b (fl/ (fl* 2.0 a) us))))))
             (cond [(or (k . fl< . 0.0) (k . fl> . n))  (loop)]
                   [(and (us . fl>= . 0.07) (v . fl<= . vr))  k]
                   [else
                    (let ([v  (fl* v (fl/ α (fl+ b (fl/ a (fl* us us)))))])
                      (define h (+ (fllog-factorial m)
                                   (fllog-factorial (fl- n m))
                                   (- (fllog-factorial k))
                                   (- (fllog-factorial (fl- n k)))
                                   (fl* (fl- k m) (fllog (fl/ p q)))))
                      (cond [((fllog v) . fl<= . h)  k]
                            [else  (loop)]))])))
         (if s? (fl- n k) k)))))

(: flbinomial-sample-normal (Flonum Flonum Natural -> FlVector))
(define (flbinomial-sample-normal n p m)
  (define q (fl- 1.0 p))
  (define μ (fl- (fl* (fl+ n 1.0) p) 0.5))
  (define σ (flsqrt (* (+ 1.0 n) p q)))
  (define γ (fl/ (fl- q p) σ))
  (build-flvector
   m (λ (_)
       (let loop ()
         (define z (unsafe-flvector-ref (flnormal-sample 0.0 1.0 1) 0))
         (define k (flround (fl+ μ (fl* σ (fl+ z (fl/ (fl* γ (fl- (fl* z z) 1.0)) 6.0))))))
         (if (and (k . fl>= . 0.0) (k . fl<= . n)) k (loop))))))

(: flbinomial-normal-appx-error-bound (Flonum Flonum -> Flonum))
;; Returns a bound on the integrated difference between the normal and binomial cdfs
;; See the Berry-Esséen theorem
(define (flbinomial-normal-appx-error-bound n p)
  (define q (fl- 1.0 p))
  (fl/ (fl* 0.4784 (fl+ (fl* p p) (fl* q q))) (flsqrt (* n p q))))

(: flbinomial-sample (Flonum Flonum Integer -> FlVector))
(define (flbinomial-sample n p m)
  (cond [(m . < . 0)  (raise-argument-error 'flbinomial-sample "Natural" 2 n p m)]
        [(or (not (integer? n)) (n . fl< . 0.0) (p . fl< . 0.0) (p . fl> . 1.0))
         (build-flvector m (λ (_) +nan.0))]
        [(or (fl= n 0.0) (fl= p 0.0))
         (build-flvector m (λ (_) 0.0))]
        [(fl= p 1.0)
         (build-flvector m (λ (_) n))]
        [(and (n . fl> . 1e8)
              ((flbinomial-normal-appx-error-bound n p) . fl< . (flexp -10.0)))
         (flbinomial-sample-normal n p m)]
        [((fl* n (flmin p (fl- 1.0 p))) . fl>= . 10.0)
         (flbinomial-sample-hormann n p m)]
        [else 
         (flbinomial-sample-small n p m)]))
