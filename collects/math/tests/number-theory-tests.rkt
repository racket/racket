#lang typed/racket
(require math/number-theory)
(require typed/rackunit)

; "quadratic.rkt"
(check-equal? (quadratic-solutions 1 0 -4) '(-2 2))
(check-equal? (quadratic-solutions 1 0 +4) '())
(check-equal? (quadratic-solutions 1 0 0)  '(0))
(check-equal? (quadratic-integer-solutions 1 0 -4) '(-2 2))
(check-equal? (quadratic-integer-solutions 1 0 +4) '())
(check-equal? (quadratic-integer-solutions 1 0 0)  '(0))
(check-equal? (quadratic-natural-solutions 1 0 -4) '(2))
(check-equal? (quadratic-natural-solutions 1 0 +4) '())
(check-equal? (quadratic-natural-solutions 1 0 0)  '(0))

; "eulerian-number.rkt"
(check-equal? (map (λ: ([x : Natural]) (eulerian-number 5 x)) '(0 1 2 3 4))
              '(1 26 66 26 1))

; "primitive-roots.rkt"
(check-equal? (unit-group 20) '(1 3 7 9 11 13 17 19))  ; 19 !!!!
(check-equal? (unit-group-order 19 20) 2)
(check-equal? (unit-group-order  3 20) 4)
(check-equal? (unit-group-orders 20) '(1 4 4 2 2 4 4 2)) ; (unit-group-order 3 20)=4, ...
(check-true   (andmap exists-primitive-root? '(1 2 4 3 9 6 18)))
(check-false  (ormap  exists-primitive-root? '(8 16 12)))
(check-equal? (primitive-root 20) #f)
(check-equal? (primitive-root 10) 7) ; (length (unit-group 10)) = (unit-group-order 7 10)
(check-true   (primitive-root? 7 10))
(check-false  (primitive-root? 7 20))
(check-equal? (primitive-roots 10) '(3 7))
(: find-and-check-root : Positive-Integer -> Boolean)
(define (find-and-check-root n)
  (define r (primitive-root n))
  (cond [(not r) #t]
        [else (= (length (unit-group n)) (unit-group-order r n))]))
(check-true   (andmap find-and-check-root '(1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18 19 78125)))

;"polygonal.rkt"
(check-equal? (map triangle-number    '(0 1 2 3 4 5)) '(0 1 3  6 10 15))
(check-equal? (map pentagonal-number  '(0 1 2 3 4 5)) '(0 1 5 12 22 35))
(check-equal? (map hexagonal-number   '(0 1 2 3 4 5)) '(0 1 6 15 28 45))
(check-equal? (map heptagonal-number  '(0 1 2 3 4 5)) '(0 1 7 18 34 55))
(check-equal? (map octagonal-number   '(0 1 2 3 4 5)) '(0 1 8 21 40 65))
(check-true   (andmap triangle-number?    '(0 1 3  6 10 15)))
(check-true   (andmap square-number?      '(0 1 4  9 16 25)))
(check-true   (andmap pentagonal-number?  '(0 1 5 12 22 35)))
(check-true   (andmap hexagonal-number?   '(0 1 6 15 28 45)))
(check-true   (andmap heptagonal-number?  '(0 1 7 18 34 55)))
(check-true   (andmap octagonal-number?   '(0 1 8 21 40 65)))

; "farey.rkt"
(check-equal? (farey-sequence 5) '(0 1/5 1/4 1/3 2/5 1/2 3/5 2/3 3/4 4/5 1))
(check-equal? (mediant 1/1 1/2) 2/3)

; "fibonacci.rkt"
(check-equal? (build-list 8 fibonacci) '(0 1 1 2 3 5 8 13))
(check-equal? (build-list 8 (make-fibonacci 2 1)) '(2 1 3 4 7 11 18 29))
(for*: ([a  (in-range -5 6)]
        [b  (in-range -5 6)]
        [mod  (in-range 1 8)])
  (check-equal? (build-list 20 (λ: ([n : Integer]) ((make-modular-fibonacci a b) n mod)))
                (build-list 20 (λ: ([n : Integer]) (modulo ((make-fibonacci a b) n) mod)))))

; "partitions.rkt"
(check-equal? (map partitions '(0 1 2 3 4 5 6 7 8 9 10))
              '(1 1 2 3 5 7 11 15 22 30 42))


; "bernoulli.rkt"
(check-equal? (map bernoulli-number '(0 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16 17 18))
              '(1 -1/2 1/6 0 -1/30 0 1/42 0 -1/30 0 5/66 0 -691/2730 0 7/6 0 -3617/510 0 43867/798))

; "tangent-number.rkt"
(require typed/rackunit)
(check-equal? (map tangent-number '(1 3 5 7 9 11 13)) '(1 2 16 272 7936 353792 22368256))
(check-equal? (map tangent-number '(0 2 4 6 8 10)) '(0 0 0 0 0 0))

; "factorial.rkt"
(define fact-table-size 171)
(define simple-cutoff 244)

(: test-factorial : Integer -> Integer)
(define (test-factorial n)
  (if (= n 0) 1 (* n (test-factorial (- n 1)))))

(check-equal? (map factorial (list 0 1 2 3 4 5)) '(1 1 2 6 24 120))
(check-equal? (factorial (+ fact-table-size 1)) (test-factorial (+ fact-table-size 1)))
(check-equal? (factorial (+ simple-cutoff 1)) (test-factorial (+ simple-cutoff 1)))

(check-equal? (binomial 10 3) 120)
(check-equal? (binomial 10 11) 0)
(check-equal? (binomial 10 0) 1)
(check-equal? (binomial 10 10) 1)
(check-equal? (binomial 10 1) 10)
(check-equal? (binomial 10 9) 10)

(check-equal? (permutations 10 3) 720)
(check-equal? (permutations 10 0) 1)
(check-equal? (permutations 10 10) 3628800)
(check-equal? (permutations 0 0) 1)

(check-equal? (multinomial 20 '(3 4 5 8)) 3491888400)
(check-equal? (multinomial 0 '()) 1)
(check-equal? (multinomial 4 '(1 1)) 0)

; "binomial.rkt"
(check-equal? (binomial 10 3) 120)
(check-equal? (binomial 10 11) 0)
(check-equal? (binomial 10 0) 1)
(check-equal? (binomial 10 10) 1)
(check-equal? (binomial 10 1) 10)
(check-equal? (binomial 10 9) 10)


; "number-theory.rkt"
(check-true  (divides? 2 12))
(check-false (divides? 2 13))
(check-true  (divides? 2 0))
; (check-exn   (divides? 0 2)) ?

(check-equal? (max-dividing-power 3 27) 3)
(check-equal? (max-dividing-power 3 (* 27 2)) 3)

(: list-dot : (Listof Integer) (Listof Integer) -> Integer)
(define (list-dot as bs)
  (if (empty? as) 
      0
      (+ (* (car as) (car bs)) (list-dot (cdr as) (cdr bs)))))

(: member? : (All (a) (a (Listof a) -> Boolean)))
(define (member? x xs)
  (not (not (member x xs))))

(check-equal? ; 2*12-1*20 = 4 = gcd(12,20)
 (list-dot '(12 20) (bezout 12 20)) (gcd 12 20))
(check-equal? (list-dot '(12 20) (bezout 12 20)) (gcd 12 20))
(check-equal? (list-dot '(20 16) (bezout 20 16)) (gcd 20 16))
(check-equal? (list-dot '(12 20 16) (bezout 12 20 16)) (gcd 12 20 16))

(check-true (coprime? (* 3 7) (* 5 19)))
(check-false (coprime? (* 3 7 5) (* 5 19 2)))

(check-true  (pairwise-coprime? 10 7 33 13))
(check-false (pairwise-coprime? 10 7 33 14))
(check-false (pairwise-coprime? 6 10 15))
(check-true  (coprime? 6 10 15))
(: check-inverse : Natural -> Boolean)
(define (check-inverse n)
  (define m (and (coprime? n 20) (modular-inverse n 20)))
  (cond [m  (= (remainder (* n m) 20) 1)]
        [else  #t]))
(check-true (andmap check-inverse (build-list 20 (λ: ([x : Natural]) (+ x 1)))))

(check-equal? (solve-chinese '(2 3 2) '(3 5 7)) 23)


(check-equal? (divisors 12)  '(1 2 3 4 6 12))
(check-equal? (divisors -12) '(1 2 3 4 6 12))
(check-equal? (divisors 0)   '())

(check-equal? (next-primes -5 10) '(-3 -2 2 3 5 7 11 13 17 19))
(check-equal? (prev-primes  5 10) '(3 2 -2 -3 -5 -7 -11 -13 -17 -19))
(check-equal? (next-prime 0) 2)
(check-equal? (next-prime 1) 2)
(check-equal? (prev-prime 10) 7)
(check-equal? (prev-prime 8) 7)
(check-equal? (prev-prime 17) 13)
(check-equal? (nth-prime 0) 2)
(check-equal? (nth-prime 1) 3)
(check-equal? (nth-prime 2) 5)
                              

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

(check-equal? (next-prime (expt 10 7)) 10000019)
(check-equal? (next-prime (expt 10 8)) 100000007)
(check-equal? (factorize (* 10000019 100000007)) '((10000019 1) (100000007 1)))
(: check-factorize : Natural -> Boolean)
(define (check-factorize n)
  (= (defactorize (factorize n)) n))
(check-true (for/and: : Boolean ([n : Natural (in-range (expt 10 9) (+ (expt 10 9) 10000))])
              (check-factorize n)))

(: check-as-power : Positive-Integer Natural Natural -> Boolean) 
(define (check-as-power a r n)
  (define-values (b e) (as-power a))
  (and (= b r) (= e n)))
(check-true (check-as-power 27 3 3))
(check-true (check-as-power 28 28 1))
(check-true (check-as-power (* 5 5 7 7 7) (* 5 5 7 7 7) 1))
(check-true (check-as-power (* 5 5 7 7 7 7) 245 2))

(check-true (prime-power? (expt 3 7)))
(check-false (prime-power? (expt 12 7)))

(check-false (perfect-power? 3))
(check-true (perfect-power? 9))
(check-true (perfect-power? (expt 12 7)))
(check-false (perfect-power? (- (expt 12 7) 1)))


(check-equal? (moebius-mu (* 3 5 7 11)) 1)
(check-equal? (moebius-mu (* 3 5 7))    -1)
(check-equal? (moebius-mu (* 3 5 5 7))  0)
 
(check-equal? (divisor-sum 1000)   2340)
(check-equal? (divisor-sum 1000 0) 16)
(check-equal? (divisor-sum 1000 1) 2340)
(check-equal? (divisor-sum 1000 2) 1383460)

(: check-integer-root : Natural Natural -> Boolean)
(define (check-integer-root a n)
  (define r (integer-root a n))
  (unless (and (<= (expt r n) a) (> (expt (+ r 1) n) a))
    (displayln (list 'check-integer-root 'a a 'n n)))
  (and (<= (expt r n) a) (> (expt (+ r 1) n) a)))
  
(for:([a : Natural (in-range (expt 10 9) (+ (expt 10 9) 10000))]
      [n : Natural (in-range 2 5)])
  (check-true (check-integer-root a n)))

; "quadratic-residues.rkt"
(check-equal? (quadratic-character  2 5) -1)
(check-equal? (quadratic-character  3 5) -1)
(check-equal? (quadratic-character  5 5)  0)
(check-equal? (quadratic-character  7 5) -1)
(check-equal? (quadratic-character 11 5)  1)

(check-true  (quadratic-residue? 1 17))
(check-true  (quadratic-residue? 2 17))
(check-true  (quadratic-residue? 4 17))
(check-true  (quadratic-residue? 8 17))
(check-true  (quadratic-residue? 9 17))
(check-true  (quadratic-residue? 13 17))
(check-true  (quadratic-residue? 15 17))
(check-true  (quadratic-residue? 16 17))
(check-false (quadratic-residue?  3 17))
(check-false (quadratic-residue?  5 17))
(check-false (quadratic-residue?  6 17))
(check-false (quadratic-residue?  7 17))
(check-false (quadratic-residue? 10 17))
(check-false (quadratic-residue? 11 17))
(check-false (quadratic-residue? 12 17))
(check-false (quadratic-residue? 14 17))
