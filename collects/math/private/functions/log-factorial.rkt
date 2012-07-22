#lang typed/racket/base

(require racket/flonum racket/fixnum
         "factorial.rkt"
         "../../constants.rkt"
         "../utils.rkt")

(provide fllog-factorial
         log-factorial
         log-binomial
         log-permutations
         log-multinomial)

(define log-fact-table-size 171)
(define log-fact-table
  (build-flvector log-fact-table-size
                  (compose real->double-flonum (compose log factorial))))

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

(: log-factorial (Integer -> Real))
(define (log-factorial n)
  (cond [(n . < . 0)  (raise-type-error 'log-factorial "Natural" n)]
        [(or (= 0 n) (= 1 n))  0]
        [else  (fllog-factorial n)]))

(: log-binomial (Integer Integer -> Real))
(define (log-binomial n k)
  (cond [(not (nonnegative-fixnum? n))  (raise-type-error 'log-binomial "Nonnegative-Fixnum" 0 n k)]
        [(not (nonnegative-fixnum? k))  (raise-type-error 'log-binomial "Nonnegative-Fixnum" 1 n k)]
        [(k . > . n)  -inf.0]
        [(zero? k)  0.0]
        [(zero? n)  -inf.0]
        [else  (- (log-factorial n) (log-factorial k) (log-factorial (- n k)))]))

(: log-permutations (Integer Integer -> Real))
(define (log-permutations n k)
  (cond [(not (nonnegative-fixnum? n))
         (raise-type-error 'log-permutations "Nonnegative-Fixnum" 0 n k)]
        [(not (nonnegative-fixnum? k))
         (raise-type-error 'log-permutations "Nonnegative-Fixnum" 1 n k)]
        [(k . > . n)  -inf.0]
        [(zero? k)  0.0]
        [(zero? n)  -inf.0]
        [else  (- (log-factorial n) (log-factorial (- n k)))]))

(: log-multinomial (Integer Integer * -> Real))
(define (log-multinomial n . ks)
  (cond [(not (nonnegative-fixnum? n))
         (raise-type-error 'log-multinomial "Nonnegative-Fixnum" 0 n ks)]
        [(not (listof-nonnegative-fixnum? ks))
         (raise-type-error 'log-multinomial "(Listof Nonnegative-Fixnum)" 1 n ks)]
        [(not (= n (apply + ks)))
         (error 'log-multinomial "expected ks to sum to n; given ~e and ~e" n ks)]
        [else  (apply - (log-factorial n) (map log-factorial ks))]))
