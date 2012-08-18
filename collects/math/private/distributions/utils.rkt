#lang typed/racket/base

(require racket/flonum)

(provide (all-defined-out))

;; ===================================================================================================
;; Conversions from standard distributions for "scale" families

(define-syntax-rule (scale-pdf f)
  (λ: ([s : Float])
    (: pdf (Float -> Float))
    (define (pdf x)
      (/ (f (/ x s)) s))
    pdf))

(define-syntax-rule (scale-log-pdf log-f)
  (λ: ([s : Float])
    (: log-pdf (Float -> Float))
    (define (log-pdf x)
      (- (log-f (/ x s)) (fllog s)))
    log-pdf))

(define-syntax-rule (scale-cdf f)
  (λ: ([s : Float])
    (: cdf (Float -> Float))
    (define (cdf x)
      (f (/ x s)))
    cdf))

(define-syntax-rule (scale-ccdf f)
  (λ: ([s : Float])
    (: ccdf (Float -> Float))
    (define (ccdf x)
      (f (/ x s)))
    ccdf))

(define-syntax-rule (scale-log-cdf log-f)
  (λ: ([s : Float])
    (: log-cdf (Float -> Float))
    (define (log-cdf x)
      (log-f (/ x s)))
    log-cdf))

(define-syntax-rule (scale-log-ccdf log-f)
  (λ: ([s : Float])
    (: log-ccdf (Float -> Float))
    (define (log-ccdf x)
      (log-f (/ x s)))
    log-ccdf))

(define-syntax-rule (scale-inv-cdf inv-f)
  (λ: ([s : Float])
    (: inv-cdf (Float -> Float))
    (define (inv-cdf p)
      (* s (inv-f p)))
    inv-cdf))

;; ===================================================================================================
;; Conversions from standard distributions for "location-scale" families

(define-syntax-rule (location-scale-pdf f)
  (λ: ([x0 : Float] [s : Float])
    (: pdf (Float -> Float))
    (define (pdf x)
      (/ (f (/ (- x x0) s)) s))
    pdf))

(define-syntax-rule (location-scale-log-pdf log-f)
  (λ: ([x0 : Float] [s : Float])
    (: log-pdf (Float -> Float))
    (define (log-pdf x)
      (- (log-f (/ (- x x0) s)) (fllog s)))
    log-pdf))

(define-syntax-rule (location-scale-cdf f)
  (λ: ([x0 : Float] [s : Float])
    (: cdf (Float -> Float))
    (define (cdf x)
      (f (/ (- x x0) s)))
    cdf))

(define-syntax-rule (location-scale-log-cdf log-f)
  (λ: ([x0 : Float] [s : Float])
    (: log-cdf (Float -> Float))
    (define (log-cdf x)
      (log-f (/ (- x x0) s)))
    log-cdf))

(define-syntax-rule (location-scale-symmetric-ccdf f)
  (λ: ([x0 : Float] [s : Float])
    (: ccdf (Float -> Float))
    (define (ccdf x)
      (f (/ (- x0 x) s)))
    ccdf))

(define-syntax-rule (location-scale-symmetric-log-ccdf log-f)
  (λ: ([x0 : Float] [s : Float])
    (: log-ccdf (Float -> Float))
    (define (log-ccdf x)
      (log-f (/ (- x0 x) s)))
    log-ccdf))

(define-syntax-rule (location-scale-inv-cdf inv-f)
  (λ: ([x0 : Float] [s : Float])
    (: inv-cdf (Float -> Float))
    (define (inv-cdf p)
      (+ x0 (* s (inv-f p))))
    inv-cdf))

;; ===================================================================================================
;; Sampling using the inverse-cdf-transform method

(define-syntax-rule (inv-cdf-random inv-cdf)
  (λ ()
    (define r (* 0.5 (random)))
    (inv-cdf (if ((random). > . 0.5) r (- r)))))
