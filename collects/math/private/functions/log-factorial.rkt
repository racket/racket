#lang typed/racket/base

(require racket/flonum racket/fixnum
         "factorial.rkt"
         "../../constants.rkt"
         "../vector/flvector.rkt"
         "../utils.rkt"
         "../exception.rkt")

(provide fllog-factorial
         fllog-binomial
         fllog-permutations
         fllog-multinomial
         log-factorial
         log-binomial
         log-permutations
         log-multinomial)

(define log-fact-table-size 171)
(define log-fact-table
  (build-flvector log-fact-table-size
                  (λ: ([n : Index])
                    (real->double-flonum (log (factorial n))))))

;; Computes log(Gamma(n+1)) using 5 terms from Stirling's series
;; For n >= 142, relative error ε < +epsilon.0
(: fllog-factorial/stirling5 (Natural -> Float))
(define (fllog-factorial/stirling5 n)
  (let* ([x  (->fl n)]
         [log-x  (fllog x)])
    (+ (* x log-x)
       (- x)
       (* 0.5 (+ (fllog (* 2.0 pi.0)) log-x))
       (let ([1/x  (/ 1.0 x)])
         (* 1/x (+ (* #i-1/360 (* 1/x 1/x)) #i1/12))))))

;; Computes log(Gamma(n+1)) using 3 terms from Stirling's series
;; For n >= 1e7, relative error ε < +epsilon.0
(: fllog-factorial/stirling3 (Natural -> Float))
(define (fllog-factorial/stirling3 n)
  (let* ([x  (->fl n)]
         [log-x  (fllog x)])
    (+ (* x log-x)
       (- x)
       (* 0.5 (+ (fllog (* 2.0 pi.0)) log-x)))))

(: fllog-factorial (Integer -> Float))
(define (fllog-factorial n)
  (cond [(n . < . 0)  +nan.0]
        [(n . < . log-fact-table-size)  (flvector-ref log-fact-table n)]
        [(n . < . #e1e7)  (fllog-factorial/stirling5 n)]
        [(n . < . #e1e306)  (fllog-factorial/stirling3 n)]
        [else  +inf.0]))

(: log-factorial (case-> (Negative-Integer -> Nothing)
                         (Zero -> Zero)
                         (One -> Zero)
                         (Integer -> (U Zero Float))))
(define (log-factorial n)
  (cond [(n . < . 0)  (raise-argument-error 'log-factorial "Natural" n)]
        [(or (eqv? n 0) (eqv? n 1))  0]
        [else  (fllog-factorial n)]))

(: fllog-binomial (Integer Integer -> Float))
(define (fllog-binomial n k)
  (cond [(not (nonnegative-fixnum? n))  +nan.0]
        [(not (nonnegative-fixnum? k))  +nan.0]
        [(zero? k)  0.0]
        [(k . > . n)  -inf.0]
        [(k . = . n)  0.0]
        [else  (- (fllog-factorial n) (fllog-factorial k) (fllog-factorial (- n k)))]))

(: log-binomial (case-> (Negative-Integer Integer -> Nothing)
                        (Integer Negative-Integer -> Nothing)
                        (Integer Zero -> Zero)
                        (Integer Integer -> Real)))
(define (log-binomial n k)
  (cond [(not (nonnegative-fixnum? n))
         (raise-argument-error 'log-binomial "Nonnegative-Fixnum" 0 n k)]
        [(not (nonnegative-fixnum? k))
         (raise-argument-error 'log-binomial "Nonnegative-Fixnum" 1 n k)]
        [(zero? k)  0]
        [(k . > . n)  -inf.0]  ; also handles n = 0 case
        [(k . = . n)  0]
        [else  (- (fllog-factorial n) (fllog-factorial k) (fllog-factorial (- n k)))]))

(: fllog-permutations (Integer Integer -> Float))
(define (fllog-permutations n k)
  (cond [(not (nonnegative-fixnum? n))  +nan.0]
        [(not (nonnegative-fixnum? k))  +nan.0]
        [(zero? k)  0.0]
        [(k . > . n)  -inf.0]  ; also handles n = 0 case
        [else  (- (fllog-factorial n) (fllog-factorial (- n k)))]))

(: log-permutations (case-> (Negative-Integer Integer -> Nothing)
                            (Integer Negative-Integer -> Nothing)
                            (Integer Zero -> Zero)
                            (Integer Integer -> Real)))
(define (log-permutations n k)
  (cond [(not (nonnegative-fixnum? n))
         (raise-argument-error 'log-permutations "Nonnegative-Fixnum" 0 n k)]
        [(not (nonnegative-fixnum? k))
         (raise-argument-error 'log-permutations "Nonnegative-Fixnum" 1 n k)]
        [(zero? k)  0]
        [(k . > . n)  -inf.0]  ; also handles n = 0 case
        [(= k n 1)  0]
        [else  (- (fllog-factorial n) (fllog-factorial (- n k)))]))

(: fllog-multinomial (Integer Integer * -> Float))
(define (fllog-multinomial n . ks)
  (cond [(not (nonnegative-fixnum? n))  +nan.0]
        [(not (andmap nonnegative-fixnum? ks))  +nan.0]
        [(not (= n (apply + ks)))  -inf.0]
        [(ormap (λ: ([k : Nonnegative-Fixnum]) (= n k)) ks)  0.0]
        [else  (apply - (fllog-factorial n) (map fllog-factorial ks))]))

(: log-multinomial (Integer Integer * -> Real))
(define (log-multinomial n . ks)
  (cond [(not (nonnegative-fixnum? n))
         (raise-argument-error 'log-multinomial "Nonnegative-Fixnum" 0 n ks)]
        [(not (andmap nonnegative-fixnum? ks))
         (raise-argument-error 'log-multinomial "(Listof Nonnegative-Fixnum)" 1 n ks)]
        [(not (= n (apply + ks)))  -inf.0]
        [(ormap (λ: ([k : Nonnegative-Fixnum]) (= n k)) ks)  0]
        [else  (apply - (fllog-factorial n) (map fllog-factorial ks))]))
