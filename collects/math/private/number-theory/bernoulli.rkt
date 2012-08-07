#lang typed/racket
(provide bernoulli)

(require "../functions/factorial.rkt")

(define-predicate natural?         Natural)
(define-predicate exact-rational?  Exact-Rational)

(: bernoulli : Natural -> Exact-Rational)
;   compute the n'th Bernoulli number
;   <http://mathworld.wolfram.com/BernoulliNumber.html>
;   <http://en.wikipedia.org/wiki/Bernoulli_number>
(define (bernoulli n)
  ; Implementation note:
  ;   - uses Ramanujan's improvement of the standard recurrence relation
  ;     of the Bernoulli numbers:
  ;     <http://en.wikipedia.org/wiki/Bernoulli_number#Ramanujan.27s_congruences>
  ;   - memoizes previous computations
  ;   - avoids an explicit call to compute the binomials
  ;   TODO: 
  ;     Memoize the computed numbers, also from call to call
  ;     Simple change - but is it the right thing to do?
  (define: b : (Vectorof (U Exact-Rational False))
    (make-vector (max (+ n 1) 2) #f))
  (vector-set! b 0 1)
  (vector-set! b 1 -1/2)
  (: next-binom : Integer Integer Integer -> Integer)
  (define (next-binom old x k)
    ; calculate binom(x,k-6) from the old binom(x,k)
    (let ([k-1 (- k 1)][k-2 (- k 2)][k-3 (- k 3)][k-4 (- k 4)][k-5 (- k 5)])
      (assert 
       (* old
          (/ (* k k-1 k-2 k-3 k-4 k-5 )
             (* (- x k-1) (- x k-2) (- x k-3) (- x k-4) (- x k-5) (- x (- k 6)))))
       integer?)))
  (: A : Integer Integer -> Exact-Rational)
  (define (A m M)
    (cond 
      [(< M 1) 0]
      [else
       (define: sum : Exact-Rational 0)
       (define: bin : Integer (binomial (+ m 3) (- m 6)))
       (for ([j (in-range 1 (+ M 1))])
         (set! sum (+ sum (* bin (bern (- m (* 6 j))))))
         (set! bin (next-binom bin (+ m 3) (- m (* 6 j)))))       
       sum]))
  (: bern : Integer -> Exact-Rational)
  (define (bern n)
    (define bn (vector-ref b n))
    (cond
      [bn bn]
      [(odd? n) 0]
      [else
       (define r (remainder n 6))
       (define bn
         (cond           
           [(= r 0) (/  (- (/ (+ n 3)  3) (A n (quotient    n    6)))  (binomial (+ n 3) n))]
           [(= r 2) (/  (- (/ (+ n 3)  3) (A n (quotient (- n 2) 6)))  (binomial (+ n 3) n))]
           [(= r 4) (/  (- (/ (+ n 3) -6) (A n (quotient (- n 4) 6)))  (binomial (+ n 3) n))]
           [else -42])) ; avoid Void in type
       ((inst vector-set! (U Exact-Rational False)) b n bn)
       bn]))
  (bern n))

