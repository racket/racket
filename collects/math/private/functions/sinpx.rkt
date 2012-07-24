#lang typed/racket/base

(require "../../constants.rkt"
         racket/flonum
         (only-in racket/math exact-floor))

(provide log-pi-minus-log-sinpx sinpx)

;; Computes x * sin(pi * x) in a way that keeps the argument to sin < 1/2*pi
(: sinpx (Float -> Float))
(define (sinpx z)
  (let ([z  (flabs z)])
    (define fl (exact-floor z))
    (define-values (dist sign)
      (cond [(odd? fl)  (values (+ 1.0 (- (->fl fl) z)) -1.0)]
            [else       (values (- z (->fl fl)) 1.0)]))
    (let ([dist  (if (dist . < . 0.5) dist (- 1.0 dist))])
      (* sign z (sin (* dist pi.0))))))

(: log-pi-minus-log-sinpx (Float -> Float))
;; Computes log(pi) - log(abs(sinpx(x))) in a way that's accurate near zero
(define (log-pi-minus-log-sinpx z)
  (let ([z  (flabs z)])
    (cond [(z . < . 1e-10)  (- (* 2.0 (fllog z)))]
          [else  (define fl (exact-floor z))
                 (define dist
                   (cond [(odd? fl)  (+ 1.0 (- (->fl fl) z))]
                         [else       (- z (->fl fl))]))
                 (let ([dist  (if (dist . < . 0.5) dist (- 1.0 dist))])
                   (- (log pi.0) (+ (fllog z) (fllog (sin (* dist pi.0))))))])))
