#lang typed/racket/base

(require racket/fixnum
         "../../flonum.rkt"
         "../../base.rkt"
         "../number-theory/factorial.rkt"
         "../vector/flvector.rkt")

(provide fllog-factorial
         fllog-binomial
         fllog-permutations
         fllog-multinomial
         log-factorial
         log-binomial
         log-permutations
         log-multinomial)

(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)

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

(: log-factorial (case-> (Zero -> Zero)
                         (One -> Zero)
                         (Integer -> (U Zero Float))))
(define (log-factorial n)
  (cond [(n . < . 0)  (raise-argument-error 'log-factorial "Natural" n)]
        [(or (eqv? n 0) (eqv? n 1))  0]
        [else  (fllog-factorial n)]))

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

(: log-binomial (case-> (Integer Zero -> Zero)
                        (One One -> Zero)
                        (Integer Integer -> (U Zero Flonum))))
(define (log-binomial n k)
  (cond [(n . < . 0)  (raise-argument-error 'log-binomial "Natural" 0 n k)]
        [(k . < . 0)  (raise-argument-error 'log-binomial "Natural" 1 n k)]
        [(zero? k)  0]
        [(k . > . n)  -inf.0]  ; also handles n = 0 case
        [(k . = . n)  0]
        [(eqv? n 1)  (if (eqv? k 1) 0 (fllog-binomial n k))]
        [else  (fllog-binomial n k)]))

(: fllog-permutations (Integer Integer -> Float))
(define (fllog-permutations n k)
  (cond [(n . < . 0)  +nan.0]
        [(k . < . 0)  +nan.0]
        [(zero? k)  0.0]
        [(k . > . n)  -inf.0]  ; also handles n = 0 case
        [else  (fl- (fllog-factorial n) (fllog-factorial (- n k)))]))

(: log-permutations (case-> (Integer Zero -> Zero)
                            (One One -> Zero)
                            (Integer Integer -> (U Zero Flonum))))
(define (log-permutations n k)
  (cond [(n . < . 0)  (raise-argument-error 'log-permutations "Natural" 0 n k)]
        [(k . < . 0)  (raise-argument-error 'log-permutations "Natural" 1 n k)]
        [(zero? k)  0]
        [(k . > . n)  -inf.0]  ; also handles n = 0 case
        [(eqv? n 1)  (if (eqv? k 1) 0 (fllog-permutations n k))]
        [else  (fllog-permutations n k)]))

(: fllog-multinomial (Integer Integer * -> Float))
(define (fllog-multinomial n . ks)
  (cond [(n . < . 0)  +nan.0]
        [(ormap negative? ks)  +nan.0]
        [(not (= n (apply + ks)))  -inf.0]
        [(ormap (λ: ([k : Integer]) (= n k)) ks)  0.0]
        [else  (apply - (fllog-factorial n) (map fllog-factorial ks))]))

(: log-multinomial (Integer Integer * -> (U Zero Flonum)))
(define (log-multinomial n . ks)
  (cond [(n . < . 0)  (raise-argument-error 'log-multinomial "Natural" 0 n ks)]
        [(ormap negative? ks)  (raise-argument-error 'log-multinomial "Natural" 1 n ks)]
        [(not (= n (apply + ks)))  -inf.0]
        [(ormap (λ: ([k : Integer]) (= n k)) ks)  0]
        [else  (apply - (fllog-factorial n) (map fllog-factorial ks))]))
