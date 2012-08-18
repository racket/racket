#lang typed/racket/base

#|
TODO

fix when p very large or very small
fix when k very large
allow p = 0.0 (reason using limits)
|#

(require racket/flonum
         racket/performance-hint
         "../../constants.rkt"
         "../functions/expm1.rkt"
         "../functions/log1p.rkt"
         "../random.rkt"
         "utils.rkt")

(provide geom-pdf geom-log-pdf
         geom-cdf geom-log-cdf
         geom-ccdf geom-log-ccdf
         geom-inv-cdf
         geom-random)

(define-syntax-rule (geom-fun name f)
  (let ()
    (: fun (Float -> (Float -> Float)))
    (define (fun p)
      (cond [(and (0.0 . < . p) (p . <= . 1.0))
             (: name  (Float -> Float))
             (define (name  k) (f p (floor k)))
             name ]
            [else
             (: name  (Float -> Float))
             (define (name  k) +nan.0)
             name ]))
    fun))

(begin-encourage-inline
  (define geom-pdf
    (geom-fun
     pdf
     (λ: ([p : Float] [k : Float])
       (cond [(k . < . 0.0)  0.0]
             [else  (* p (flexpt (- 1.0 p) (floor k)))]))))
  
  (define geom-log-pdf
    (geom-fun
     log-pdf
     (λ: ([p : Float] [k : Float])
       (cond [(k . < . 0.0)  -inf.0]
             [else  (+ (fllog p) (* k (fllog1p (- p))))]))))
  
  (define geom-cdf
    (geom-fun
     cdf
     (λ: ([p : Float] [k : Float])
       (cond [(k . < . 0.0)  0.0]
             [else  (- 1.0 (flexpt (- 1.0 p) (+ k 1.0)))]))))
  
  (define geom-ccdf
    (geom-fun
     ccdf
     (λ: ([p : Float] [k : Float])
       (cond [(k . < . 0.0)  1.0]
             [else  (flexpt (- 1.0 p) (+ k 1.0))]))))
  
  (define geom-log-cdf
    (geom-fun
     log-cdf
     (λ: ([p : Float] [k : Float])
       (cond [(k . < . 0.0)  -inf.0]
             [else  (fllog1p (- (flexpt (- 1.0 p) (+ k 1.0))))]))))
  
  (define geom-log-ccdf
    (geom-fun
     log-ccdf
     (λ: ([p : Float] [k : Float])
       (cond [(k . < . 0.0)  0.0]
             [else  (* (+ k 1.0) (fllog1p (- p)))]))))
  
  (: geom-inv-cdf (Float -> (Float -> Float)))
  (define (geom-inv-cdf p)
    (cond
      [(and (0.0 . < . p) (p . <= . 1.0))
       (: inv-cdf (Float -> Float))
       (define (inv-cdf q)
         (cond [(q . > . 1.0)  +nan.0]
               [(q . = . 1.0)  +inf.0]
               [(q . > . 0.0)
                (define log-1-q (fllog1p (- q)))
                (define log-1-p (fllog1p (- p)))
                (abs (max 0.0 (ceiling (/ (- log-1-q log-1-p) log-1-p))))]
               [(q . = . 0.0)  (if (eqv? q -0.0) +inf.0 0.0)]
               [(q . > . -1.0)
                (define log-1-q (fllog (- q)))
                (define log-p (fllog1p (- p)))
                (abs (max 0.0 (ceiling (/ (- log-1-q log-p) log-p))))]
               [(q . = . -1.0)  0.0]
               [else  +nan.0]))
       inv-cdf]
      [else
       (: inv-cdf (Float -> Float))
       (define (inv-cdf k) +nan.0)
       inv-cdf]))
  
  (: geom-random (Float -> (-> Float)))
  (define (geom-random s)
    (: random (-> Float))
    (define random (inv-cdf-random (geom-inv-cdf s)))
    random)
  )  ; begin-encourage-inline
