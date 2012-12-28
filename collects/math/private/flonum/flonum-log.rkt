#lang typed/racket/base

(require racket/performance-hint
         (only-in racket/math pi)
         "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt"
         "flonum-error.rkt"
         "flvector.rkt")

(provide fllog1p fllog+
         lg1+ lg+ lg1- lg- lgsum
         fllog-quotient)

(begin-encourage-inline
  
  (: fllog1p (Float -> Float))
  ;; Computes the value of log(1+x) in a way that is accurate for small x
  (define (fllog1p x)
    (define ax (flabs x))
    (cond [(ax . fl>= . 1.0)  (fllog (fl+ 1.0 x))]
          [(ax . fl>= . (fl* 0.5 epsilon.0))
           (define y (fl+ 1.0 x))
           (fl- (fllog y) (fl/ (fl- (fl- y 1.0) x) y))]
          [else  x]))
  
  (: fllog+ (Flonum Flonum -> Flonum))
  ;; Computes log(a+b) in a way that is accurate for a+b near 1.0
  (define (fllog+ a b)
    (define a+b (+ a b))
    (cond [((flabs (- a+b 1.0)) . < . (fllog 2.0))
           ;; a+b is too close to 1.0, so compute in higher precision
           (define-values (a+b a+b-lo) (fast-fl+/error a b))
           (- (fllog a+b) (fllog1p (- (/ a+b-lo a+b))))]
          [(a+b . = . +inf.0)
           ;; a+b overflowed, so reduce the arguments
           (+ (fllog 2.0) (fllog (+ (* 0.5 a) (* 0.5 b))))]
          [else
           (fllog a+b)]))
  
  (: lg1+ (Float -> Float))
  (define (lg1+ log-x)
    (cond [(log-x . fl>= . 0.0)  (fl+ log-x (fllog1p (flexp (- log-x))))]
          [else  (fllog1p (flexp log-x))]))
  
  (: lg+ (Float Float -> Float))
  (define (lg+ log-x log-y)
    (let ([log-x  (flmax log-x log-y)]
          [log-y  (flmin log-x log-y)])
      (cond [(fl= log-x -inf.0)  -inf.0]
            [else  (fl+ log-x (fllog1p (flexp (fl- log-y log-x))))])))
  
  (: lg1- (Float -> Float))
  (define (lg1- log-x)
    (cond [(log-x . fl> . (fllog 0.5))  (fllog (- (flexpm1 log-x)))]
          [else  (fllog1p (- (flexp log-x)))]))
  
  (: lg- (Float Float -> Float))
  (define (lg- log-x log-y)
    (cond [(log-x . fl< . log-y)  +nan.0]
          [(fl= log-x -inf.0)  -inf.0]
          [else  (fl+ log-x (lg1- (fl- log-y log-x)))]))
  
  )  ; begin-encourage-inline

(: flmax* ((Listof Flonum) -> Flonum))
(define (flmax* xs)
  (let loop ([xs xs] [mx -inf.0])
    (if (null? xs) mx (loop (cdr xs) (flmax mx (car xs))))))

(: lgsum ((Listof Flonum) -> Flonum))
(define (lgsum log-xs)
  (if (null? log-xs)
      0.0
      (let ([log-x0  (car log-xs)]
            [log-xs  (cdr log-xs)])
        (if (null? log-xs)
            log-x0
            (let ([log-x1  (car log-xs)]
                  [log-xs  (cdr log-xs)])
              (if (null? log-xs)
                  (lg+ log-x0 log-x1)
                  (let ([max-log-x  (flmax (flmax log-x0 log-x1) (flmax* log-xs))])
                    (if (fl= max-log-x -inf.0)
                        -inf.0
                        (let ([s  (flsum
                                   (list* -1.0  ; for the max element; faster than removing it
                                          (flexp (- log-x0 max-log-x))
                                          (flexp (- log-x1 max-log-x))
                                          (map (λ: ([log-x : Flonum]) (flexp (- log-x max-log-x)))
                                               log-xs)))])
                          ;; Yes, we subtract 1.0 and then add 1.0 before taking the log; this
                          ;; helps with precision a bit when s is near zero
                          (+ max-log-x (fllog1p s)))))))))))

(: fllog-quotient (Flonum Flonum -> Flonum))
;; Computes (fllog (/ x y)) in a way that reduces error and avoids under-/overflow
(define (fllog-quotient x y)
  (let ([x  (flabs x)]
        [y  (flabs y)]
        [s  (fl/ (flsgn x) (flsgn y))])
    (cond [(s . fl> . 0.0)
           (define z (fl/ x y))
           (cond [(and (z . fl> . +max-subnormal.0) (z . fl< . +inf.0))  (fllog (fl* s z))]
                 [else  (fl+ (fllog x) (- (fllog y)))])]
          [(s . fl= . 0.0)  -inf.0]
          [else  +nan.0])))
