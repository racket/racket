#lang typed/racket/base

(require racket/flonum
         racket/performance-hint
         "../../types.rkt"
         "../../constants.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "../inline-sort.rkt"
         "utils.rkt")

(provide fltriangle-pdf
         fltriangle-cdf
         fltriangle-inv-cdf
         fltriangle-random
         triangle-pdf
         triangle-cdf
         triangle-inv-cdf
         triangle-random)

(: fltriangle-pdf (Float Float Float Float Any -> Float))
(define (fltriangle-pdf a b c x log?)
  (let-values ([(a c b)  (inline-sort < a b c)])
    (define p
      (cond [(x . < . a)  0.0]
            [(x . < . c)  (/ (* 2.0 (- x a))
                             (* (- b a) (- c a)))]
            [(x . < . b)  (/ (* 2.0 (- b x))
                             (* (- b a) (- b c)))]
            [else  0.0]))
    (if log? (fllog p) p)))

(: fltriangle-cdf (Float Float Float Float Any Any -> Float))
(define (fltriangle-cdf a b c x log? upper-tail?)
  (let-values ([(a c b)  (inline-sort < a b c)])
    (define q
      (cond [(x . < . a)  0.0]
            [(x . < . c)  (define x-a (- x a))
                          (/ (* x-a x-a)
                             (* (- b a) (- c a)))]
            [(x . < . b)  (define b-x (- b x))
                          (- 1.0 (/ (* b-x b-x)
                                    (* (- b a) (- b c))))]
            [else  1.0]))
    (cond [upper-tail?  (if log? (fllog (- 1.0 q)) (- 1.0 q))]
          [else  (if log? (fllog q) q)])))

(: fltriangle-inv-cdf (Float Float Float Float Any Any -> Float))
(define (fltriangle-inv-cdf a b c q log? upper-tail?)
  (let-values ([(a c b)  (inline-sort < a b c)])
    (let ([q  (cond [upper-tail?  (if log? (- 1.0 (exp q)) (- 1.0 q))]
                    [else  (if log? (exp q) q)])])
      (cond [(q . < . 0.0)  +nan.0]
            [(q . = . 0.0)  a]
            [(q . < . (/ (- c a) (- b a)))
             ;; a < x < c
             (+ a (flsqrt (* (- c a) (- b a) q)))]
            [(q . < . 1.0)
             ;; c < x < b
             (- b (flsqrt (* (- b c) (- b a) (- 1.0 q))))]
            [(q . = . 1.0)  b]
            [else  +nan.0]))))

(: fltriangle-random (Float Float Float -> Float))
(define (fltriangle-random a b c)
  (fltriangle-inv-cdf a b c (* 0.5 (random)) #f ((random) . > . 0.5)))

(begin-encourage-inline
  
  (: triangle-pdf (case-> (-> Real-Density-Function)
                          (Real -> Real-Density-Function)
                          (Real Real -> Real-Density-Function)
                          (Real Real Real -> Real-Density-Function)))
  (define (triangle-pdf [a 0.0] [b 1.0] [c (* 0.5 (+ a b))])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)]
          [c  (real->double-flonum c)])
      (: pdf Real-Density-Function)
      (define (pdf x [log? #f])
        (fltriangle-pdf a b c (real->double-flonum x) log?))
      pdf))
  
  (: triangle-cdf (case-> (-> Real-Distribution-Function)
                          (Real -> Real-Distribution-Function)
                          (Real Real -> Real-Distribution-Function)
                          (Real Real Real -> Real-Distribution-Function)))
  (define (triangle-cdf [a 0.0] [b 1.0] [c (* 0.5 (+ a b))])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)]
          [c  (real->double-flonum c)])
      (: cdf Real-Distribution-Function)
      (define (cdf x [log? #f] [upper-tail? #f])
        (fltriangle-cdf a b c (real->double-flonum x) log? upper-tail?))
      cdf))
  
  (: triangle-inv-cdf (case-> (-> Real-Distribution-Function)
                              (Real -> Real-Distribution-Function)
                              (Real Real -> Real-Distribution-Function)
                              (Real Real Real -> Real-Distribution-Function)))
  (define (triangle-inv-cdf [a 0.0] [b 1.0] [c (* 0.5 (+ a b))])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)]
          [c  (real->double-flonum c)])
      (: inv-cdf Real-Distribution-Function)
      (define (inv-cdf q [log? #f] [upper-tail? #f])
        (fltriangle-inv-cdf a b c (real->double-flonum q) log? upper-tail?))
      inv-cdf))
  
  (: triangle-random (case-> (-> (-> Float))
                             (Real -> (-> Float))
                             (Real Real -> (-> Float))
                             (Real Real Real -> (-> Float))))
  (define (triangle-random [a 0.0] [b 1.0] [c (* 0.5 (+ a b))])
    (let ([a  (real->double-flonum a)]
          [b  (real->double-flonum b)]
          [c  (real->double-flonum c)])
      (λ () (fltriangle-random a b c))))
  
  )  ; begin-encourage-inline
