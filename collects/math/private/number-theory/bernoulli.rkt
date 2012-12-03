#lang typed/racket/base

(require "../vector/vector.rkt"
         "types.rkt"
         "factorial.rkt"
         "binomial.rkt")

(provide bernoulli-number)

;; Number of globally memoized Bernoulli numbers
(define num-global-bs 200)
;; Globally memoized numbers
(: global-bs (Vectorof Exact-Rational))
(define global-bs (make-vector num-global-bs 0))
(vector-set! global-bs 0 1)
(vector-set! global-bs 1 -1/2)

(: bernoulli* : Natural -> Exact-Rational)
;   compute the n'th Bernoulli number
;   <http://mathworld.wolfram.com/BernoulliNumber.html>
;   <http://en.wikipedia.org/wiki/Bernoulli_number>
(define (bernoulli* n)
  ; Implementation note:
  ;   - uses Ramanujan's improvement of the standard recurrence relation
  ;     of the Bernoulli numbers:
  ;     <http://en.wikipedia.org/wiki/Bernoulli_number#Ramanujan.27s_congruences>
  ;   - memoizes previous computations
  ;   - avoids an explicit call to compute the binomials
  (define: local-bs : (Vectorof Exact-Rational)
    (make-vector (max 0 (- (+ n 1) num-global-bs)) 0))
  
  (: bs-ref! (Integer (-> Exact-Rational) -> Exact-Rational))
  (define (bs-ref! n thnk)
    (cond [(n . < . num-global-bs)
           (vector-ref! global-bs n thnk exact-zero?)]
          [else
           (vector-ref! local-bs (- n num-global-bs) thnk exact-zero?)]))
  
  (: next-binom : Integer Integer Integer -> Integer)
  (define (next-binom old x k)
    ; calculate binom(x,k-6) from the old binom(x,k)
    (let ([k-1 (- k 1)] [k-2 (- k 2)] [k-3 (- k 3)] [k-4 (- k 4)] [k-5 (- k 5)])
      (assert 
       (* old
          (/ (* k k-1 k-2 k-3 k-4 k-5 )
             (* (- x k-1) (- x k-2) (- x k-3) (- x k-4) (- x k-5) (- x (- k 6)))))
       integer?)))
  
  (: A : Natural Integer -> Exact-Rational)
  (define (A m M)
    (cond 
      [(< M 1) 0]
      [else
       (define: m-6 : Natural (assert (- m 6) natural?))
       (define-values (sum bin)
         (for/fold: ([sum : Exact-Rational  0]
                     [bin : Integer  (binomial (+ m 3) m-6)]
                     ) ([j  (in-range 1 (+ M 1))])
           (values (+ sum (* bin (bern (- m (* 6 j)))))
                   (next-binom bin (+ m 3) (- m (* 6 j))))))
       sum]))
  
  (: bern : Integer -> Exact-Rational)
  (define (bern n)
    (bs-ref!
     n (λ ()
         (cond
           [(odd? n)  0]
           [else
            (define r (remainder n 6))
            (cond           
              [(= r 0) (/  (- (/ (+ n 3)  3) (A n (quotient    n    6)))  (binomial (+ n 3) n))]
              [(= r 2) (/  (- (/ (+ n 3)  3) (A n (quotient (- n 2) 6)))  (binomial (+ n 3) n))]
              [(= r 4) (/  (- (/ (+ n 3) -6) (A n (quotient (- n 4) 6)))  (binomial (+ n 3) n))]
              ;; n is even, so r can only be 0, 2 or 4
              [else  (error 'unreachable-code)])]))))
  (bern n))

(: bernoulli-number (Integer -> Exact-Rational))
(define (bernoulli-number n)
  (cond [(n . < . 0)  (raise-argument-error 'bernoulli-number "Natural" n)]
        [else  (bernoulli* n)]))
