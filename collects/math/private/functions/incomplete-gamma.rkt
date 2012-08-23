#lang typed/racket/base

(require racket/flonum
         racket/fixnum
         math/bigfloat
         "../../flonum.rkt"
         "continued-fraction.rkt"
         "log1p.rkt"
         "gamma.rkt"
         "log-gamma.rkt"
         "log-arithmetic.rkt")

(provide flgamma-lower
         fllog-gamma-lower
         flgamma-upper
         fllog-gamma-upper
         gamma-lower
         log-gamma-upper
         gamma-upper
         log-gamma-upper)

(: flgamma-lower-iter (Float Float -> Float))
(define (flgamma-lower-iter k x)
  (let: loop : Float ([y  : Float  0.0]
                      [dy : Float  (/ x (+ k 1.0))]
                      [i  : Float  0.0])
    (define new-y (+ y dy))
    (cond [(or (= new-y +inf.0) ((abs dy) . <= . (* +epsilon.0 new-y)))  new-y]
          [else  (loop new-y (/ (* dy x) (+ 2.0 i k)) (+ i 1.0))])))

(: k^x/k*exp-x (Float Float -> Float))
;; Computes k^x/k * exp(-x) in a way that avoids overflow and (hopefully) reduces error
(define (k^x/k*exp-x k x)
  (define log-z (- (* k (fllog x)) (+ x (fllog k))))
  ;; Use exp(log(z)) if precise enough
  (cond [((abs log-z) . < . 1.0)  (exp log-z)]
        [else
         ;; Arranged to reduce x^k immediately:
         (define z (/ (* (flexpt x k) (exp (- x))) k))
         (cond [(rational? z)  z]
               [else
                ;; Arranged to avoid overflow:
                (define x^k/2 (flexpt x (* 0.5 k)))
                (* (/ (* x^k/2 (exp (- x))) k) x^k/2)])]))

(: log-k^x/k*exp-x (Float Float -> Float))
;; Computes k^x/k * exp(-x) in a way that avoids overflow and (hopefully) reduces error
(define (log-k^x/k*exp-x k x)
  ;; Arranged to reduce x^k immediately:
  (define z (/ (* (flexpt x k) (exp (- x))) k))
  (cond [(rational? z)  (fllog z)]
        [else
         ;; Arranged to avoid overflow:
         (define x^k/2 (flexpt x (* 0.5 k)))
         (define z (* (/ (* x^k/2 (exp (- x))) k) x^k/2))
         (cond [(rational? z)  (fllog z)]
               [else
                ;; Won't overflow easily, but subject to cancellation errors:
                (- (* k (fllog x)) (+ x (fllog k)))])]))

(: flgamma-lower-series (Float Float -> Float))
;; Computes the lower gamma function from its series
(define (flgamma-lower-series k x)
  (define y (flgamma-lower-iter k x))
  (define z (k^x/k*exp-x k x))
  (cond [(y . < . 1.0)  (+ z (* z y))]  ; avoid adding 1.0 if y is near zero
        [else  (* z (+ y 1.0))]))

(: fllog-gamma-lower-series (Float Float -> Float))
;; Computes the log of the lower gamma function from its series
(define (fllog-gamma-lower-series k x)
  (define y (flgamma-lower-iter k x))
  (define log-z (log-k^x/k*exp-x k x))
  (+ log-z (fllog1p y)))

;; ===================================================================================================

(: k^x*exp-x (Float Float -> Float))
;; Computes k^x * exp(-x) in a way that avoids overflow and (hopefully) reduces error
(define (k^x*exp-x k x)
  (define log-z (- (* k (fllog x)) x))
  ;; Use exp(log(z)) if precise enough
  (cond [((abs log-z) . < . 1.0)  (exp log-z)]
        [else
         (define z (* (flexpt x k) (exp (- x))))
         (cond [(rational? z)  z]
               [else
                ;; Arranged to avoid overflow:
                (define x^k/2 (flexpt x (* 0.5 k)))
                (* (* x^k/2 (exp (- x))) x^k/2)])]))

(: log-k^x*exp-x (Float Float -> Float))
;; Computes log(k^x * exp(-x)) in a way that avoids overflow and (hopefully) reduces error
(define (log-k^x*exp-x k x)
  (define z (* (flexpt x k) (exp (- x))))
  (cond [(rational? z)  (fllog z)]
        [else
         ;; Arranged to avoid overflow:
         (define x^k/2 (flexpt x (* 0.5 k)))
         (define z (* (* x^k/2 (exp (- x))) x^k/2))
         (cond [(rational? z)  (fllog z)]
               [else
                ;; Won't overflow easily, but subject to cancellation errors:
                (- (* k (fllog x)) x)])]))

(: flgamma-upper-iter (Float Float -> Float))
(define (flgamma-upper-iter k x)
  (continued-fraction 1.0
                      (λ (i a) (* i (- k i)))
                      (+ 1.0 (- x k))
                      (λ (i b) (+ (- x k) (* 2.0 i) 1.0))
                      +epsilon.0))

(: flgamma-upper-frac (Float Float -> Float))
;; Computes the upper gamma function using Legendre's continued fraction
(define (flgamma-upper-frac k x)
  (define y (flgamma-upper-iter k x))
  (define z (k^x*exp-x k x))
  (* y z))

(: fllog-gamma-upper-frac (Float Float -> Float))
;; Computes the log of the upper gamma function using Legendre's continued fraction
(define (fllog-gamma-upper-frac k x)
  (define y (flgamma-upper-iter k x))
  (define log-z (log-k^x*exp-x k x))
  (+ log-z (fllog y)))

;; ===================================================================================================

(: use-lower? (Float Float -> Boolean))
;; Determines whether to compute an incomplete gamma function using the lower's series or upper's
;; continued fraction
(define (use-lower? k x)
  (or (x . < . k) (and (x . < . 4.0) (k . < . 3.0))))

(: flgamma-lower (Float Float -> Float))
(define (flgamma-lower k x)
  (cond [(k . <= . 0.0)  +nan.0]
        [(x . <  . 0.0)  +nan.0]
        [(use-lower? k x)  (flgamma-lower-series k x)]
        [else  (- (flgamma k) (flgamma-upper-frac k x))]))

(: fllog-gamma-lower (Float Float -> Float))
(define (fllog-gamma-lower k x)
  (cond [(k . <= . 0.0)  +nan.0]
        [(x . <  . 0.0)  +nan.0]
        [(use-lower? k x)  (fllog-gamma-lower-series k x)]
        [else  (fllog- (fllog-gamma k) (fllog-gamma-upper-frac k x))]))

(: flgamma-upper (Float Float -> Float))
(define (flgamma-upper k x)
  (cond [(k . <= . 0.0)  +nan.0]
        [(x . <  . 0.0)  +nan.0]
        [(use-lower? k x)  (- (flgamma k) (flgamma-lower-series k x))]
        [else  (flgamma-upper-frac k x)]))

(: fllog-gamma-upper (Float Float -> Float))
(define (fllog-gamma-upper k x)
  (cond [(k . <= . 0.0)  +nan.0]
        [(x . <  . 0.0)  +nan.0]
        [(use-lower? k x)  (fllog- (fllog-gamma k) (fllog-gamma-lower-series k x))]
        [else  (fllog-gamma-upper-frac k x)]))

;; ===================================================================================================

(define-syntax-rule (define-incomplete-gamma-wrapper name flname)
  (begin
    (: name (case-> (Single-Flonum Single-Flonum -> Single-Flonum)
                    (Float Float -> Float)
                    (Real Real -> Real)))
    (define (name k x)
      (cond [(and (double-flonum? k) (double-flonum? x))  (flname x k)]
            [(and (single-flonum? k) (single-flonum? x))
             (real->single-flonum (flname (real->double-flonum k) (real->double-flonum x)))]
            [else
             (flname (real->double-flonum k) (real->double-flonum x))]))))

(define-incomplete-gamma-wrapper gamma-lower flgamma-lower)
(define-incomplete-gamma-wrapper log-gamma-lower fllog-gamma-lower)
(define-incomplete-gamma-wrapper gamma-upper flgamma-upper)
(define-incomplete-gamma-wrapper log-gamma-upper fllog-gamma-upper)
