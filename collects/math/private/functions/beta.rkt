#lang typed/racket/base

(require "../../flonum.rkt"
         "../../base.rkt"
         "gamma.rkt"
         "log-gamma.rkt"
         "stirling-error.rkt"
         "lanczos.rkt")

(provide flbeta fllog-beta beta log-beta)

(define: flbeta-hash : (HashTable (Pair Flonum Flonum) Flonum) (make-weak-hash))
(define: fllog-beta-hash : (HashTable (Pair Flonum Flonum) Flonum) (make-weak-hash))

(: flbeta-limits (Flonum Flonum -> Flonum))
;; Assumes a >= b (or one is +nan.0)
(define (flbeta-limits a b)
  (cond [(or (a . < . 0.0) (b . < . 0.0))  +nan.0]
        [(and (= a +inf.0) (= b 0.0))  +nan.0]
        [(or (= a 0.0) (= b 0.0))  +inf.0]
        [(or (= a +inf.0) (= b +inf.0))  0.0]
        [else  +nan.0]))

(: flbeta (Flonum Flonum -> Flonum))
(define (flbeta a b)
  (let ([a  (flmax a b)]
        [b  (flmin a b)])
    (cond [(not (and (b . fl> . 0.0) (a . fl< . +inf.0)))
           (flbeta-limits a b)]
          [(fl= a 1.0)  (fl/ 1.0 b)]
          [(fl= b 1.0)  (fl/ 1.0 a)]
          [(b . fl> . 540.0)  0.0]
          ;; Asymptotic expansion for small `b' and large `a', derived from Stirling's approximation
          ;; Domain where this has low error (<= 3 ulps) was found by experimentation (for the first
          ;; condition), and by solving for a^-b = small (for the second)
          [(or (and (b . fl< . 1.0) (a . fl> . (fl* 1e16 b)))
               (and (a . fl> . 1e17) (b . fl< . (fl/ 700.0 (fllog a)))))
           (fl* (flgamma b) (flexpt a (- b)))]
          ;; Use direct implementation when it doesn't under-/overflow
          [(and (b . fl>= . 1.0) (a . fl<= . 99.0) ((fl+ a b) . fl<= . 171.0))
           (fl/ (fl* (flgamma a) (flgamma b)) (flgamma (fl+ a b)))]
          [else
           (hash-ref!
            flbeta-hash (cons a b)
            (λ ()
              (define-values (a/b a/b-lo) (fast-fl//error a b))
              (define-values (b/a b/a-lo) (fast-fl//error b a))
              (cond
                ;; Use extended-precision implementation based on Stirling's series when it
                ;; won't under-/overflow
                [(and (a/b . fl> . +max-subnormal.0) (a/b . fl< . +inf.0)
                      (b/a . fl> . +max-subnormal.0) (b/a . fl< . +inf.0))
                 (define-values (1+a/b 1+a/b-lo) (fast-fl+/error 1.0 a/b))
                 (define-values (1+b/a 1+b/a-lo) (fast-fl+/error 1.0 b/a))
                 (* (fl/ (fl* (flexp-stirling a) (flexp-stirling b))
                         (flexp-stirling (fl+ a b)))
                    (flsqrt (fl/ (* 2.0 pi (fl+ a b)) (fl* a b)))
                    (flexpt+ 1+a/b (fl+ 1+a/b-lo a/b-lo) (- b))
                    (flexpt+ 1+b/a (fl+ 1+b/a-lo b/a-lo) (- a)))]
                [else
                 (* (fl/ (fl* (flexp-stirling a) (flexp-stirling b))
                         (flexp-stirling (fl+ a b)))
                    (flsqrt (fl/ (fl/ (* 2.0 pi (fl+ a b)) a) b))
                    (flexpt (fl/ (fl+ a b) b) (- b))
                    (flexpt (fl/ (fl+ b a) a) (- a)))])))])))

(: fllog-beta-stirling (Flonum Flonum -> Flonum))
(define (fllog-beta-stirling a b)
  (define t
    (let ([t  (flstirling (fl+ a b))])
      (cond [(t . < . +inf.0)  t]
            [else  (/ #i1/24 (+ (* 0.5 a) (* 0.5 b)))])))
  (+ (fl- (fl+ (flstirling a) (flstirling b)) t)
     (* 0.5 (- (+ (fllog (* 2.0 pi)) (fllog+ a b)) (fllog a) (fllog b)))
     (* a (- (fllog a) (fllog+ a b)))
     (* b (- (fllog b) (fllog+ a b)))))

(: fllog-beta (Flonum Flonum -> Flonum))
(define (fllog-beta a b)
  (let ([a  (flmax a b)]
        [b  (flmin a b)])
    (cond [(not (and (b . fl> . 0.0) (a . fl< . +inf.0)))
           (fllog (flbeta-limits a b))]
          [(fl= a 1.0)  (- (fllog b))]
          [(fl= b 1.0)  (- (fllog a))]
          [else
           (define y-est (fl+ (fllog-gamma b) (fl* (- b) (fllog a))))
           (cond
             ;; If an overestimate of (fllog-beta a b) is small enough but not too small, or large
             ;; enough and rational, then (fllog (flbeta a b)) has low error
             ;; The "too small" value log(1e-220) was determined experimentally
             [(or (and (y-est . > . (fllog 1e-220)) (y-est . < . (fllog #i1/8)))
                  (and (y-est . > . (fllog 8.0)) (y-est . < . (fllog +max.0))))
              (fllog (flbeta a b))]
             [(y-est . > . (flexp 1.0))  (fllog-beta-stirling a b)]
             [else
              (hash-ref!
               fllog-beta-hash (cons a b)
               (λ ()
                 (define-values (a/b a/b-lo) (fast-fl//error a b))
                 (define-values (b/a b/a-lo) (fast-fl//error b a))
                 (cond
                   ;; Use extended-precision implementation based on Stirling's series when it won't
                   ;; under-/overflow
                   [(and (a/b . fl> . +max-subnormal.0) (a/b . fl< . +inf.0)
                         (b/a . fl> . +max-subnormal.0) (b/a . fl< . +inf.0)
                         (a/b-lo . fl> . +max-subnormal.0) (a/b-lo . fl< . +inf.0)
                         (b/a-lo . fl> . +max-subnormal.0) (b/a-lo . fl< . +inf.0))
                    (define-values (1+a/b 1+a/b-lo) (fast-fl+/error 1.0 a/b))
                    (define-values (1+b/a 1+b/a-lo) (fast-fl+/error 1.0 b/a))
                    (define t
                      (let ([t  (* 0.5 (fllog (fl/ (* 2.0 pi (fl+ a b)) (fl* a b))))])
                        (cond [(and (t . > . -inf.0) (t . < . +inf.0))  t]
                              [else  (* 0.5 (- (+ (fllog (* 2.0 pi)) (fllog+ a b))
                                               (+ (fllog a) (fllog b))))])))
                    (+ t (fl- (fl+ (flstirling a) (flstirling b))
                            (flstirling (fl+ a b)))
                       (* (- b) (fllog+ 1+a/b (fl+ 1+a/b-lo a/b-lo)))
                       (* (- a) (fllog+ 1+b/a (fl+ 1+b/a-lo b/a-lo))))]
                   [else
                    (fllog-beta-stirling a b)])))])])))

;; ===================================================================================================

(: log-beta (case-> (One One -> Zero)
                    (Flonum Flonum -> Flonum)
                    (Real Real -> (U Zero Flonum))))
(define (log-beta a b)
  (cond [(and (exact? a) (a . <= . 0))
         (raise-argument-error 'log-beta "positive Real" 0 a b)]
        [(and (exact? b) (b . <= . 0))
         (raise-argument-error 'log-beta "positive Real" 1 a b)]
        [(eqv? a 1)
         (if (eqv? b 1) 0 (fllog-beta (fl a) (fl b)))]
        [else
         (fllog-beta (fl a) (fl b))]))

(: beta (case-> (Positive-Integer Positive-Integer -> Exact-Rational)
                (Flonum Flonum -> Flonum)
                (Real Real -> (U Exact-Rational Flonum))))
(define (beta a b)
  (cond [(and (exact? a) (a . <= . 0))
         (raise-argument-error 'beta "positive Real" 0 a b)]
        [(and (exact? b) (b . <= . 0))
         (raise-argument-error 'beta "positive Real" 1 a b)]
        [(exact-integer? a)
         (if (exact-integer? b)
             (/ (* (gamma a) (gamma b)) (gamma (+ a b)))
             (flbeta (fl a) (fl b)))]
        [else
         (flbeta (fl a) (fl b))]))
