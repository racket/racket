#lang typed/racket/base

(require "../../flonum.rkt"
         "../../constants.rkt"
         "../exception.rkt"
         "log-gamma.rkt"
         "lanczos.rkt"
         "log1p.rkt")

(: fllog-beta-huge (Flonum Flonum -> Flonum))
;; For a,b > 8e307 (when the Lanczos approximation would overflow)
;; Assumes a >= b
(define (fllog-beta-huge a b)
  (cond [((/ a b) . > . 1e6)
         (cond [(b . < . 1.0)
                (- (fllog-gamma (+ b 2.0))
                   (fllog (* b (flexpt a b)))
                   (fllog1p b))]
               [else
                (- (fllog-gamma b)
                   (* b (fllog a)))])]
        [else
         ;; Stirling's approximation for when a ~ b
         (+ (* a (fllog (/ (* a 0.5)
                           (+ (* a 0.5) (* b 0.5)))))
            (* b (fllog (/ (* b 0.5)
                           (+ (* a 0.5) (* b 0.5)))))
            (* -0.5 (- (+ (fllog a) (fllog b))
                       (fllog (* 2.0 pi.0))
                       (fllog 2.0)
                       (fllog (+ (* a 0.5) (* b 0.5))))))]))

(: fllog-beta-small (Flonum Flonum -> Flonum))
;; For a,b < 10
;; Assumes a >= b
(define (fllog-beta-small a b)
  (cond [(b . < . 1.0)
         (- (fllog-gamma (+ b 2.0))
            (+ (fllog b) (- (fllog-gamma (+ a b)) (fllog-gamma a)))
            (fllog1p b))]
        [else
         (+ (- (fllog-gamma a) (fllog-gamma (+ a b)))
            (fllog-gamma b))]))

(: fllog-beta-lanczos (Flonum Flonum -> Flonum))
;; Assumes a >= b
(define (fllog-beta-lanczos a b)
  (define x (+ a lanczos-g -0.5))
  (define y (+ b lanczos-g -0.5))
  (define z (+ y a))
  (+ (* (- a b 0.5) (fllog1p (* -2.0 (/ b (- (* 2.0 (+ a b lanczos-g)) 1.0)))))
     (* b (+ (fllog (/ x z)) (fllog (/ y z))))
     (* 0.5 (- 1.0 (fllog y) (* 2.0 lanczos-g)))
     (fllog (/ (* (lanczos-sum a) (lanczos-sum b))
               (lanczos-sum (+ a b))))))

(: fllog-beta (Flonum Flonum -> Flonum))
(define (fllog-beta a b)
  (let ([a  (max a b)]
        [b  (min a b)])
    (cond [(or (a . < . 0.0) (b . < . 0.0))  +nan.0]
          [(or (a . = . 0.0) (b . = . 0.0))  +inf.0]
          [(and (a . < . +inf.0) (b . < . +inf.0))
           (cond [(= a 1.0)  (- (fllog b))]
                 [(= b 1.0)  (- (fllog a))]
                 [(or (a . > . 8e307) (b . > . 8e307))  (fllog-beta-huge a b)]
                 [(and (a . < . 10.0) (b . < . 10.0))  (fllog-beta-small a b)]
                 [(b . < . 1.0)  (+ (fllog (+ a b))
                                    (- (fllog-beta a (+ b 1.0))
                                       (fllog b)))]
                 [else  (fllog-beta-lanczos a b)])]
          [(or (= a +inf.0) (= b +inf.0))  -inf.0]
          [else  +nan.0])))

(: flbeta (Flonum Flonum -> Flonum))
(define (flbeta a b)
  (exp (fllog-beta a b)))

(: log-beta (Real Real -> Flonum))
(define (log-beta a b)
  (cond [(and (exact? a) (a . <= . 0))
         (raise-argument-error 'log-beta "positive Real" 0 a b)]
        [(and (exact? b) (b . <= . 0))
         (raise-argument-error 'log-beta "positive Real" 1 a b)]
        [else
         (fllog-beta (fl a) (fl b))]))

(: beta (Real Real -> Flonum))
(define (beta a b)
  (cond [(and (exact? a) (a . <= . 0))
         (raise-argument-error 'beta "positive Real" 0 a b)]
        [(and (exact? b) (b . <= . 0))
         (raise-argument-error 'beta "positive Real" 1 a b)]
        [else
         (flbeta (fl a) (fl b))]))
