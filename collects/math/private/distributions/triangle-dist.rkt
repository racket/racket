#lang typed/racket/base

(require racket/performance-hint
         "../../flonum.rkt"
         "../inline-sort.rkt"
         "utils.rkt"
         "types.rkt")

(provide fltriangle-pdf
         fltriangle-cdf
         fltriangle-inv-cdf
         fltriangle-random
         Triangular-Distribution triangle-dist triangle-dist?
         triangle-dist-min triangle-dist-center triangle-dist-max)

(: unsafe-fltriangle-pdf (Float Float Float Float Any -> Float))
(define (unsafe-fltriangle-pdf a b c x log?)
  (define p
    (cond [(x . < . a)  0.0]
          [(x . < . c)  (/ (* 2.0 (- x a))
                           (* (- b a) (- c a)))]
          [(x . < . b)  (/ (* 2.0 (- b x))
                           (* (- b a) (- b c)))]
          [else  0.0]))
  (if log? (fllog p) p))

(: unsafe-fltriangle-cdf (Float Float Float Float Any Any -> Float))
(define (unsafe-fltriangle-cdf a b c x log? upper-tail?)
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
        [else  (if log? (fllog q) q)]))

(: unsafe-fltriangle-inv-cdf (Float Float Float Float Any Any -> Float))
(define (unsafe-fltriangle-inv-cdf a b c q log? upper-tail?)
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
          [else  +nan.0])))

(: unsafe-fltriangle-random (Float Float Float -> Float))
(define (unsafe-fltriangle-random a b c)
  (unsafe-fltriangle-inv-cdf a b c (* 0.5 (random)) #f ((random) . > . 0.5)))

(begin-encourage-inline
  
  (: fltriangle-pdf (Float Float Float Float Any -> Float))
  (define (fltriangle-pdf a b c x log?)
    (let-values ([(a c b)  (inline-sort < a b c)])
      (unsafe-fltriangle-pdf a b c x log?)))
  
  (: fltriangle-cdf (Float Float Float Float Any Any -> Float))
  (define (fltriangle-cdf a b c x log? upper-tail?)
    (let-values ([(a c b)  (inline-sort < a b c)])
      (unsafe-fltriangle-cdf a b c x log? upper-tail?)))
  
  (: fltriangle-inv-cdf (Float Float Float Float Any Any -> Float))
  (define (fltriangle-inv-cdf a b c x log? upper-tail?)
    (let-values ([(a c b)  (inline-sort < a b c)])
      (unsafe-fltriangle-inv-cdf a b c x log? upper-tail?)))
  
  (: fltriangle-random (Float Float Float -> Float))
  (define (fltriangle-random a b c)
    (let-values ([(a c b)  (inline-sort < a b c)])
      (unsafe-fltriangle-random a b c)))
  
  )

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: triangle-dist
    Triangular-Distribution Real-Distribution ([min : Float] [max : Float] [center : Float]))
  
  (: triangle-dist (case-> (-> Triangular-Distribution)
                           (Real -> Triangular-Distribution)
                           (Real Real -> Triangular-Distribution)
                           (Real Real Real -> Triangular-Distribution)))
  (define (triangle-dist [a 0.0] [b 1.0] [c (* 0.5 (+ a b))])
    (let ([a  (fl a)] [b  (fl b)] [c  (fl c)])
      (let-values ([(a c b)  (inline-sort < a c b)])
        (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                      (unsafe-fltriangle-pdf a b c (fl x) log?)))
        (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [upper-tail? : Any #f])
                      (unsafe-fltriangle-cdf a b c (fl x) log? upper-tail?)))
        (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [upper-tail? : Any #f])
                          (unsafe-fltriangle-inv-cdf a b c (fl p) log? upper-tail?)))
        (define (random) (unsafe-fltriangle-random a b c))
        (make-triangle-dist pdf cdf inv-cdf random a b c))))
  
  )
