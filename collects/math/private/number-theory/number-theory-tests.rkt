#lang typed/racket
(require typed/rackunit)

(require "fibonacci-lucas.rkt")
(check-equal? ((inst map Natural Natural) fibonacci '(0 1 2 3 4 5 6 7))
              '(0 1 1 2 3 5 8 13))
(check-equal? ((inst map Natural Natural) lucas '(0 1 2 3 4 5 6 7))
              '(1 3 4 7 11 18 29 47))
(check-equal? ((inst map Natural Natural) (λ: ([x : Natural]) (fibonacci-mod x 7)) '(0 1 2 3 4 5 6 7))
              ((inst map Natural Natural) (λ: ([x : Natural]) (modulo (fibonacci x) 7)) '(0 1 2 3 4 5 6 7)))

(require "partitions.rkt")
(check-equal? ((inst map Natural Integer) partitions '(0 1 2 3 4 5 6 7 8 9 10))
              '(1 1 2 3 5 7 11 15 22 30 42))


(require "bernoulli.rkt")
(check-equal? (map bernoulli '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
              '(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730 0 7/6 0 -3617/510 0 43867/798))

(require "bernoulli-via-tangent.rkt")
(check-equal? (map bernoulli/tangent '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
              '(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730 0 7/6 0 -3617/510 0 43867/798))

(require "tangent-number.rkt")
(require typed/rackunit)
(check-equal? (map tangent-number '(1 3 5 7 9 11 13)) '(1 2 16 272 7936 353792 22368256))
(check-equal? (map tangent-number '(0 2 4 6 8 10)) '(0 0 0 0 0 0))

(require "binomial.rkt")
(check-equal? (binomial 10 3) 120)
(check-equal? (binomial 10 11) 0)
(check-equal? (binomial 10 0) 1)
(check-equal? (binomial 10 10) 1)
(check-equal? (binomial 10 1) 10)
(check-equal? (binomial 10 9) 10)


(require "number-theory.rkt")
(check-true  (divides? 2 12))
(check-false (divides? 2 13))
(check-true  (divides? 2 0))
; (check-exn   (divides? 0 2)) ?

(: list-dot : (Listof Integer) (Listof Integer) -> Integer)
(define (list-dot as bs)
  (if (empty? as) 
      0
      (+ (* (car as) (car bs)) (list-dot (cdr as) (cdr bs)))))

(: member? : (All (a) (a (Listof a) -> Boolean)))
(define (member? x xs)
  (not (not (member x xs))))

(check-equal? ; 2*12-1*20 = 4 = gcd(12,20)
 (list-dot '(12 20) (bezout-binary 12 20)) (gcd 12 20))
(check-equal? (list-dot '(12 20) (bezout 12 20)) (gcd 12 20))
(check-equal? (list-dot '(20 16) (bezout 20 16)) (gcd 20 16))
(check-equal? (list-dot '(12 20 16) (bezout 12 20 16)) (gcd 12 20 16))

(check-true (coprime? (* 3 7) (* 5 19)))
(check-false (coprime? (* 3 7 5) (* 5 19 2)))

(check-true  (pairwise-coprime? 10 7 33 13))
(check-false (pairwise-coprime? 10 7 33 14))
(check-false (pairwise-coprime? 6 10 15))
(check-true  (coprime? 6 10 15))

(check-equal? (divisors 12)  '(1 3 2 6 4 12))
(check-equal? (divisors -12) '(1 3 2 6 4 12))
(check-equal? (divisors 0)   '())

(check-equal? (next-primes -5 10) '(-3 -2 2 3 5 7 11 13 17 19))
(check-equal? (prev-primes  5 10) '(3 2 -2 -3 -5 -7 -11 -13 -17 -19))

(let ()
  (: prime-sum : Integer Integer -> Integer)
  (define (prime-sum start delta)
    (define: s : Integer 0)
    (for: ([i (in-range start (+ start delta 1))])
      (set! s (+ s (next-prime i))))
    s)
  (check-equal? (prime-sum (expt 10 6) 1000) 1001511919)    ; Sum[NextPrime[n], {n, 10^6, 10^6 + 1000}]
  (check-equal? (prime-sum (expt 10 7) 1000) 10010514423)   ; Sum[NextPrime[n], {n, 10^7, 10^7 + 1000}]
  (check-equal? (prime-sum (expt 10 8) 1000) 100100519271)  ; Sum[NextPrime[n], {n, 10^8, 10^8 + 1000}]
  (check-equal? (prime-sum (expt 10 9) 1000) 1001000516807) ; Sum[NextPrime[n], {n, 10^9, 10^9 + 1000}]
  )

#;(check-equal? 7472966967499
                (let ()
                  ; sum of the first million primes
                  (define: s : Integer 0)
                  (define: p : Integer 1)
                  (for: ([i (in-range 0 (expt 10 6))])
                    (when (zero? (remainder i 10000))
                      (newline)
                      (display i))
                    (when (zero? (remainder i 1000))
                      (display "."))
                    (set! p (next-prime p))
                    (set! s (+ s p)))
                  s))
