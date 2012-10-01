#lang typed/racket/base

(require "../../flonum.rkt"
         "../../base.rkt"
         "../exception.rkt"
         "log-gamma.rkt"
         "lanczos.rkt")

(provide flbeta fllog-beta beta log-beta)

(: fllog-beta-huge (Flonum Flonum -> Flonum))
;; For a,b > 8e307 (when the Lanczos approximation would overflow)
;; Assumes a >= b
(define (fllog-beta-huge a b)
  (cond [((fl/ a b) . fl> . 1e6)
         (cond [(b . fl< . 1.0)
                (fl- (fl- (fllog-gamma (fl+ b 2.0))
                          (fllog (fl* b (flexpt a b))))
                     (fllog1p b))]
               [else
                (fl- (fllog-gamma b)
                     (fl* b (fllog a)))])]
        [else
         ;; Stirling's approximation for when a ~ b
         (define ab/2 (fl+ (fl* a 0.5) (fl* b 0.5)))
         (fl+ (fl+ (fl* a (fllog (fl/ (fl* a 0.5) ab/2)))
                   (fl* b (fllog (fl/ (fl* b 0.5) ab/2))))
              (fl* -0.5 (fl- (fl- (fl- (fl+ (fllog a) (fllog b))
                                       (fllog (fl* 2.0 pi)))
                                  (fllog 2.0))
                             (fllog ab/2))))]))

(: fllog-beta-small (Flonum Flonum -> Flonum))
;; For a,b < 10
;; Assumes a >= b
(define (fllog-beta-small a b)
  (cond [(b . fl< . 1.0)
         (fl- (fl- (fllog-gamma (fl+ b 2.0))
                   (fl+ (fllog b) (fl- (fllog-gamma (fl+ a b)) (fllog-gamma a))))
              (fllog1p b))]
        [else
         (fl+ (fl- (fllog-gamma a) (fllog-gamma (fl+ a b)))
              (fllog-gamma b))]))

(: fllog-beta-lanczos (Flonum Flonum -> Flonum))
;; Assumes a >= b
(define (fllog-beta-lanczos a b)
  (define x (fl+ a (fl- lanczos-g 0.5)))
  (define y (fl+ b (fl- lanczos-g 0.5)))
  (define z (fl+ y a))
  (fl+ (fl+ (fl+ (fl* (fl- (fl- a b) 0.5)
                      (fllog1p (fl* -2.0 (fl/ b (fl- (fl* 2.0 (fl+ (fl+ a b) lanczos-g))
                                                     1.0)))))
                 (fl* b (fl+ (fllog (fl/ x z)) (fllog (fl/ y z)))))
            (fl* 0.5 (fl- (fl- 1.0 (fllog y)) (fl* 2.0 lanczos-g))))
       (fllog (fl/ (fl* (lanczos-sum a) (lanczos-sum b))
                   (lanczos-sum (fl+ a b))))))

(define: fllog-beta-hash : (HashTable (Pair Float Float) Float) (make-weak-hash))

(: fllog-beta (Flonum Flonum -> Flonum))
(define (fllog-beta a b)
  (let ([a  (flmax a b)]
        [b  (flmin a b)])
    (cond [(or (a . fl< . 0.0) (b . fl< . 0.0))  +nan.0]
          [(or (a . fl= . 0.0) (b . fl= . 0.0))  +inf.0]
          [(and (a . fl< . +inf.0) (b . fl< . +inf.0))
           (cond [(fl= a 1.0)  (- (fllog b))]
                 [(fl= b 1.0)  (- (fllog a))]
                 [else
                  (hash-ref!
                   fllog-beta-hash (cons a b)
                   (λ ()
                     (cond [(or (a . fl> . 8e307) (b . fl> . 8e307))  (fllog-beta-huge a b)]
                           [(and (a . fl< . 10.0) (b . fl< . 10.0))  (fllog-beta-small a b)]
                           [(b . fl< . 1.0)  (fl+ (fllog (fl+ a b))
                                                  (fl- (fllog-beta a (fl+ b 1.0))
                                                       (fllog b)))]
                           [else  (fllog-beta-lanczos a b)])))])]
          [(or (fl= a +inf.0) (fl= b +inf.0))  -inf.0]
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
