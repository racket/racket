#lang typed/racket/base

(require racket/performance-hint
         (only-in racket/math pi)
         "../vector/flvector.rkt"
         "../number-theory/factorial.rkt"
         "flonum-functions.rkt"
         "flonum-constants.rkt"
         "flonum-exp.rkt"
         "flonum-sum.rkt")

(provide fllog1p
         lg1+ lg+ lg1- lg- lgsum
         fllog-quotient
         fllog-factorial
         fllog-permutations
         fllog-binomial
         fllog-multinomial)

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
    (cond [(log-y . fl> . log-x)  +nan.0]
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

;; ===================================================================================================
;; Log-factorial and friends

(define log-fact-table-size 171)
(define log-fact-table
  (build-flvector log-fact-table-size
                  (λ: ([n : Index])
                    (real->double-flonum (log (factorial n))))))

;; Computes log(Gamma(n+1)) using 5 terms from Stirling's series
;; For n >= 142, relative error ε <= epsilon.0
(: fllog-factorial/stirling5 (Natural -> Float))
(define (fllog-factorial/stirling5 n)
  (let* ([x  (->fl n)]
         [log-x  (fllog x)])
    (fl+ (fl+ (fl- (fl* x log-x) x)
              (fl* 0.5 (fl+ (fllog (fl* 2.0 pi)) log-x)))
         (let ([1/x  (fl/ 1.0 x)])
           (fl* 1/x (fl+ (fl* #i-1/360 (fl* 1/x 1/x)) #i1/12))))))

;; Computes log(Gamma(n+1)) using 3 terms from Stirling's series
;; For n >= 1e7, relative error ε <= epsilon.0
(: fllog-factorial/stirling3 (Natural -> Float))
(define (fllog-factorial/stirling3 n)
  (let* ([x  (->fl n)]
         [log-x  (fllog x)])
    (fl+ (fl- (* x log-x) x)
         (fl* 0.5 (fl+ (fllog (fl* 2.0 pi)) log-x)))))

(: fllog-factorial (Integer -> Float))
(define (fllog-factorial n)
  (cond [(n . < . 0)  +nan.0]
        [(n . < . log-fact-table-size)  (flvector-ref log-fact-table n)]
        [(n . < . #e1e7)  (fllog-factorial/stirling5 n)]
        [(n . < . #e1e306)  (fllog-factorial/stirling3 n)]
        [else  +inf.0]))

(: fllog-binomial (Integer Integer -> Float))
(define (fllog-binomial n k)
  (cond [(n . < . 0)  +nan.0]
        [(k . < . 0)  +nan.0]
        [(zero? k)  0.0]
        [(k . > . n)  -inf.0]
        [(k . = . n)  0.0]
        [else  (fl- (fl- (fllog-factorial n)
                         (fllog-factorial k))
                    (fllog-factorial (- n k)))]))

(: fllog-permutations (Integer Integer -> Float))
(define (fllog-permutations n k)
  (cond [(n . < . 0)  +nan.0]
        [(k . < . 0)  +nan.0]
        [(zero? k)  0.0]
        [(k . > . n)  -inf.0]  ; also handles n = 0 case
        [else  (fl- (fllog-factorial n) (fllog-factorial (- n k)))]))

(: fllog-multinomial (Integer Integer * -> Float))
(define (fllog-multinomial n . ks)
  (cond [(n . < . 0)  +nan.0]
        [(ormap negative? ks)  +nan.0]
        [(not (= n (apply + ks)))  -inf.0]
        [(ormap (λ: ([k : Integer]) (= n k)) ks)  0.0]
        [else  (apply - (fllog-factorial n) (map fllog-factorial ks))]))
