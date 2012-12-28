#lang typed/racket/base

(require racket/fixnum
         racket/performance-hint
         (only-in racket/math pi)
         "../number-theory/factorial.rkt"
         "../functions/stirling-error.rkt"
         "flonum-functions.rkt"
         "flonum-log.rkt"
         "flonum-more-functions.rkt"
         "flonum-error.rkt")

(provide flfactorial
         flbinomial
         flpermutations
         flmultinomial
         fllog-factorial
         fllog-permutations
         fllog-binomial
         fllog-multinomial)

;; ===================================================================================================
;; Factorial

(begin-encourage-inline

  (: flfactorial (Flonum -> Flonum))
  ;; Error = 0 ulps
  (define (flfactorial n)
    (cond [(not (integer? n))  +nan.0]
          [(n . fl< . 0.0)  +nan.0]
          [(n . fl< . 171.0)  (fl (factorial (fl->fx n)))]
          [else  +inf.0]))
  
  (: fllog-factorial (Flonum -> Flonum))
  ;; Error <= 1 ulp
  (define (fllog-factorial n)
    (cond [(not (integer? n))  +nan.0]
          [(n . fl< . 0.0)  +nan.0]
          [(n . fl< . 171.0)  (fllog (fl (factorial (fl->fx n))))]
          [else  (+ (flstirling n)
                    (* 0.5 (fllog (* 2.0 pi n)))
                    (* n (- (fllog n) 1.0)))]))

  )  ; begin-encourage-inline

;; ===================================================================================================
;; Binomial

(: flbinomial (Flonum Flonum -> Flonum))
;; Error <= 4 ulps
(define (flbinomial n k)
  (cond [(not (integer? n))  +nan.0]
        [(not (integer? k))  +nan.0]
        [(n . fl< . 0.0)   +nan.0]
        [(k . fl<= . 0.0)  (if (fl= k 0.0) 1.0 +nan.0)]
        [(k . fl>= . n)    (if (fl= k n) 1.0 0.0)]
        [(k . fl> . (fl/ n 2.0))  (flbinomial n (fl- n k))]
        [(n . fl< . 171.0)  (flround (/ (flfactorial n) (flfactorial k) (flfactorial (- n k))))]
        [else
         (define n-k (- n k))
         (define-values (a-hi a-lo) (fast-fl//error n-k k))
         (define-values (b-hi b-lo) (fast-fl//error n n-k))
         (flround
          (* (flexp (- (flstirling n) (flstirling k) (flstirling n-k)))
             (flsqrt (/ (/ (/ n k) n-k) (fl* 2.0 pi)))
             (flexpt+ a-hi a-lo k)
             (flexpt+ b-hi b-lo n)))]))

(: fllog-binomial (Flonum Flonum -> Flonum))
;; Error <= 2 ulps
(define (fllog-binomial n k)
  (cond [(not (integer? n))  +nan.0]
        [(not (integer? k))  +nan.0]
        [(n . fl< . 0.0)   +nan.0]
        [(k . fl<= . 0.0)  (if (fl= k 0.0) 0.0 +nan.0)]
        [(k . fl>= . n)    (if (fl= k n) 0.0 -inf.0)]
        [(k . fl> . (fl/ n 2.0))  (fllog-binomial n (fl- n k))]
        [else
         (define n-k (- n k))
         (define a (* k (fllog (/ n-k k))))
         (define b (* n (fllog1p (/ k n-k))))
         (cond [((+ a b) . < . (fllog 1e300))  (fllog (flbinomial n k))]
               [else
                (+ (- (flstirling n) (flstirling k) (flstirling n-k))
                   (* 0.5 (fllog (/ (/ (/ n k) n-k) (fl* 2.0 pi))))
                   a b)])]))

;; ===================================================================================================
;; Permutations

(: flpermutations-stirling (Flonum Flonum -> Flonum))
(define (flpermutations-stirling n k)
  (define-values (a-hi a-lo) (fast-fl//error (+ n 1.0) (+ n (- 1.0 k))))
  (* (flexp (- k))
     (flexpt+ n (- 1.0 k) k)
     (flexpt+ a-hi a-lo n)
     (flexpt+ a-hi a-lo 0.5)
     (flexp (- (flstirling (+ n 1.0))
               (flstirling (+ n (- 1.0 k)))))))

(: flpermutations (Flonum Flonum -> Flonum))
;; Error <= 3 ulps
(define (flpermutations n k)
  (cond [(not (integer? n))  +nan.0]
        [(not (integer? k))  +nan.0]
        [(n . fl< . 0.0)   +nan.0]
        [(k . fl<= . 0.0)  (if (fl= k 0.0) 1.0 +nan.0)]
        [(k . fl> . n)  0.0]  ; also handles n = 0 case
        [(k . fl> . 171.0)  +inf.0]
        [(n . fl< . 171.0)  (flround (/ (flfactorial n) (flfactorial (- n k))))]
        [(n . fl< . 9e15)  (flround (flpermutations-stirling n k))]
        [(k . fl> . 19.0)  +inf.0]
        [else
         ;; Adding 1.0 to `n' no longer changes it; switch to exact for this
         ;; There's probably a faster way...
         (let loop ([z 1] [n  (fl->exact-integer n)] [k  (fl->exact-integer k)])
           (cond [(k . > . 0)  (loop (* z n) (- n 1) (- k 1))]
                 [else  (fl z)]))]))

(: fllog-permutations (Flonum Flonum -> Flonum))
;; Error <= 2 ulps
(define (fllog-permutations n k)
  (cond [(not (integer? n))  +nan.0]
        [(not (integer? k))  +nan.0]
        [(n . fl< . 0.0)  +nan.0]
        [(k . fl<= . 0.0)  (if (fl= k 0.0) 0.0 +nan.0)]
        [(k . fl> . n)  -inf.0]  ; also handles n = 0 case
        [(n . fl< . 171.0)  (fllog (flround (fl/ (fl (factorial (fl->fx n)))
                                                 (fl (factorial (fl->fx (- n k)))))))]
        [else
         (define n-k (fl- n k))
         (define a (fl* (fl+ n 0.5) (fllog1p (fl/ k (fl+ n (fl- 1.0 k))))))
         (define b (fl* k (fl- (fllog1p n-k) 1.0)))
         (cond [((fl+ a b) . fl<= . (fllog 1e300))  (fllog (flpermutations n k))]
               [else  (+ (fl- (flstirling (fl+ n 1.0)) (flstirling (fl+ n (fl- 1.0 k))))
                         a b)])]))

;; ===================================================================================================
;; Multinomial

(: fllog-multinomial (Flonum (Listof Flonum) -> Flonum))
(define (fllog-multinomial n ks)
  (cond [(n . < . 0)  +nan.0]
        [(ormap negative? ks)  +nan.0]
        [(not (= n (apply + ks)))  -inf.0]
        [(ormap (λ: ([k : Flonum]) (= n k)) ks)  0.0]
        [else  (apply - (fllog-factorial n) (map fllog-factorial ks))]))

(: flmultinomial (Flonum (Listof Flonum) -> Flonum))
(define (flmultinomial n ks)
  (flexp (fllog-multinomial n ks)))
