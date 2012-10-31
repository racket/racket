#lang typed/racket/base

(require racket/performance-hint
         racket/promise
         racket/unsafe/ops
         "../../flonum.rkt"
         "../inline-sort.rkt"
         "dist-struct.rkt"
         "utils.rkt")

(provide fltriangle-pdf
         fltriangle-cdf
         fltriangle-inv-cdf
         fltriangle-random
         Triangular-Dist triangle-dist triangle-dist?
         triangle-dist-min triangle-dist-max triangle-dist-center)

(: flsort3 (Flonum Flonum Flonum -> (Values Flonum Flonum Flonum)))
(begin-encourage-inline
  (define (flsort3 a b c) (inline-sort unsafe-fl< a b c)))

(: unsafe-fltriangle-pdf (Float Float Float Float Any -> Float))
(define (unsafe-fltriangle-pdf a b c x log?)
  (define p
    (cond [(x . fl< . a)  0.0]
          [(x . fl< . c)  (fl/ (fl* 2.0 (fl- x a))
                               (fl* (fl- b a) (fl- c a)))]
          [(x . fl< . b)  (fl/ (fl* 2.0 (fl- b x))
                               (fl* (fl- b a) (fl- b c)))]
          [else  0.0]))
  (if log? (fllog p) p))

(: unsafe-fltriangle-cdf (Float Float Float Float Any Any -> Float))
(define (unsafe-fltriangle-cdf a b c x log? 1-p?)
  (define q
    (cond [(x . fl< . a)  0.0]
          [(x . fl< . c)  (define x-a (fl- x a))
                          (fl/ (fl* x-a x-a)
                               (fl* (fl- b a) (fl- c a)))]
          [(x . fl< . b)  (define b-x (fl- b x))
                          (fl- 1.0 (fl/ (fl* b-x b-x)
                                        (fl* (fl- b a) (fl- b c))))]
          [else  1.0]))
  (cond [1-p?  (if log? (fllog (fl- 1.0 q)) (fl- 1.0 q))]
        [else  (if log? (fllog q) q)]))

(: unsafe-fltriangle-inv-cdf (Float Float Float Float Any Any -> Float))
(define (unsafe-fltriangle-inv-cdf a b c q log? 1-p?)
  (let ([q  (cond [1-p?  (if log? (fl- 1.0 (flexp q)) (fl- 1.0 q))]
                  [else  (if log? (flexp q) q)])])
    (cond [(q . fl< . 0.0)  +nan.0]
          [(q . fl= . 0.0)  a]
          [(q . fl< . (fl/ (fl- c a) (fl- b a)))
           ;; a < x < c
           (fl+ a (flsqrt (fl* (fl* (fl- c a) (fl- b a)) q)))]
          [(q . fl< . 1.0)
           ;; c < x < b
           (fl- b (flsqrt (fl* (fl* (fl- b c) (fl- b a)) (fl- 1.0 q))))]
          [(q . fl= . 1.0)  b]
          [else  +nan.0])))

(: unsafe-fltriangle-random (Float Float Float -> Float))
(define (unsafe-fltriangle-random a b c)
  (unsafe-fltriangle-inv-cdf a b c (fl* 0.5 (random)) #f ((random) . fl> . 0.5)))

(begin-encourage-inline
  
  (: fltriangle-pdf (Float Float Float Float Any -> Float))
  (define (fltriangle-pdf a b c x log?)
    (let-values ([(a c b)  (flsort3 a b c)])
      (unsafe-fltriangle-pdf a b c x log?)))
  
  (: fltriangle-cdf (Float Float Float Float Any Any -> Float))
  (define (fltriangle-cdf a b c x log? 1-p?)
    (let-values ([(a c b)  (flsort3 a b c)])
      (unsafe-fltriangle-cdf a b c x log? 1-p?)))
  
  (: fltriangle-inv-cdf (Float Float Float Float Any Any -> Float))
  (define (fltriangle-inv-cdf a b c x log? 1-p?)
    (let-values ([(a c b)  (flsort3 a b c)])
      (unsafe-fltriangle-inv-cdf a b c x log? 1-p?)))
  
  (: fltriangle-random (Float Float Float -> Float))
  (define (fltriangle-random a b c)
    (let-values ([(a c b)  (flsort3 a b c)])
      (unsafe-fltriangle-random a b c)))
  
  )

;; ===================================================================================================
;; Distribution object

(begin-encourage-inline
  
  (define-distribution-type: Triangular-Dist (Ordered-Dist Real Flonum)
    triangle-dist ([min : Float] [max : Float] [center : Float]))
  
  (: triangle-dist (case-> (-> Triangular-Dist)
                           (Real -> Triangular-Dist)
                           (Real Real -> Triangular-Dist)
                           (Real Real Real -> Triangular-Dist)))
  (define (triangle-dist [a 0.0] [b 1.0] [c (* 0.5 (+ a b))])
    (let ([a  (fl a)] [b  (fl b)] [c  (fl c)])
      (let-values ([(a c b)  (flsort3 a b c)])
        (define pdf (opt-lambda: ([x : Real] [log? : Any #f])
                      (unsafe-fltriangle-pdf a b c (fl x) log?)))
        (define cdf (opt-lambda: ([x : Real] [log? : Any #f] [1-p? : Any #f])
                      (unsafe-fltriangle-cdf a b c (fl x) log? 1-p?)))
        (define inv-cdf (opt-lambda: ([p : Real] [log? : Any #f] [1-p? : Any #f])
                          (unsafe-fltriangle-inv-cdf a b c (fl p) log? 1-p?)))
        (define (random) (unsafe-fltriangle-random a b c))
        (make-triangle-dist pdf random cdf inv-cdf
                            a b (delay (unsafe-fltriangle-inv-cdf a b c 0.5 #f #f))
                            a b c))))
  
  )
