#lang typed/racket/base

(require "../../flonum.rkt"
         "../../base.rkt")

(provide log-pi-minus-log-sinpx sinpx)

;; Computes x * sin(pi * x) in a way that keeps the argument to sin < 1/2*pi
(: sinpx (Float -> Float))
(define (sinpx z)
  (let ([z  (flabs z)])
    (define fl (exact-floor z))
    (define-values (dist sign)
      (cond [(odd? fl)  (values (fl+ 1.0 (fl- (->fl fl) z)) -1.0)]
            [else       (values (fl- z (->fl fl)) 1.0)]))
    (let ([dist  (if (dist . fl< . 0.5) dist (fl- 1.0 dist))])
      (fl* (fl* sign z) (flsin (fl* dist pi))))))

(: log-pi-minus-log-sinpx (Float -> Float))
;; Computes log(pi) - log(abs(sinpx(x))) in a way that's accurate near zero
(define (log-pi-minus-log-sinpx z)
  (let ([z  (flabs z)])
    (cond [(z . fl< . 1e-10)  (- (fl* 2.0 (fllog z)))]
          [else
           (define fl (exact-floor z))
           (define dist
             (cond [(odd? fl)  (fl+ 1.0 (fl- (->fl fl) z))]
                   [else       (fl- z (->fl fl))]))
           (let ([dist  (if (dist . fl< . 0.5) dist (fl- 1.0 dist))])
             (fl- (fllog pi) (fl+ (fllog z) (fllog (flsin (fl* dist pi))))))])))
