#lang typed/racket/base

(require "types.rkt")

(provide binomial)

(: binomial* : Natural Natural -> Natural)
(define (binomial* n k)
  ;  compute the binomial coeffecient n choose k
  ;  http://www.gnu.org/software/gmp/manual/html_node/Binomial-Coefficients-Algorithm.html
  (assert
   (let: loop : Exact-Rational ([n : Exact-Rational  n] [k : Exact-Rational  k])
     (cond
       [(= k 0)       1]    
       [(= k 1)       n]
       [(> k n)       0]
       [(= k 2)       (/ (* n (- n 1)) 2)]
       [(> k (/ n 2)) (loop n (- n k))]
       [else          (* (+ n (- k) 1)
                         (for/fold: ([prod : Exact-Rational  1]) ([i  (in-range 2 (+ k 1))])
                           (* prod (/ (+ n (- k) i) i))))]))
   natural?))

(: binomial (case-> (Integer Zero -> One)
                    (One One -> One)
                    (Integer Integer -> Natural)))
(define (binomial n k)
  (cond [(n . < . 0)  (raise-argument-error 'binomial "Natural" 0 n k)]
        [(k . < . 0)  (raise-argument-error 'binomial "Natural" 1 n k)]
        [(zero? k)  1]
        [(eqv? n 1)  (if (eqv? k 1) 1 (binomial* n k))]
        [else  (binomial* n k)]))
