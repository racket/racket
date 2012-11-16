#lang typed/racket

(require "../base/base-random.rkt"
         "divisibility.rkt"
         "modular-arithmetic.rkt"
         "types.rkt")

(require/typed typed/racket
               [integer-sqrt/remainder (Natural -> (Values Natural Natural))])

(provide solve-chinese
         
         ; primes
         nth-prime
         next-prime untyped-next-prime
         next-primes
         prev-prime untyped-prev-prime
         prev-primes
         prime?
         odd-prime?
         factorize
         defactorize
         divisors
         prime-divisors
         prime-exponents
         
         ; roots
         integer-root
         integer-root/remainder         
         
         ; Powers
         max-dividing-power
         perfect-power
         perfect-power?
         prime-power
         prime-power?
         odd-prime-power?
         as-power
         perfect-square

         ; number theoretic functions
         totient
         moebius-mu
         divisor-sum
         )

;;;
;;; Configuration
;;;

(define prime-strong-pseudo-certainty 1/10000000)
(define prime-strong-pseudo-trials
  (integer-length (assert (/ 1 prime-strong-pseudo-certainty) integer?)))

(define *SMALL-PRIME-LIMIT* 1000000)
; (define *SMALL-PRIME-LIMIT* 1000) ; use 1000 for coverage testing
; Determines the size of the pre-built table of small primes
(define *SMALL-FACORIZATION-LIMIT* *SMALL-PRIME-LIMIT*)
; Determines whether to use naive factorization or Pollards rho method.


;;;
;;; Powers
;;;

(: max-dividing-power : Z Z -> N)
; (max-dividing-power p n) = m  <=> p^m | n  and  p^(m+1) doesn't divide n
;   In Mathematica this one is called IntegerExponent
(define (max-dividing-power p n)
  (: find-start : Z Z -> Z)
  (define (find-start p-to-e e)
    ;(display (list 'fs 'p-to-e p-to-e  'e e)) (newline)
    ; p-to-e divides n  and  p-to-e = p^e
    (let ([p-to-e2 (sqr p-to-e)])
      (cond [(= p-to-e2 n) (* 2 e)]
            [(> p-to-e2 n) (find-power p-to-e e)]
            [(divides? p-to-e2 n) (if (divides? p (quotient n p-to-e2))
                                      (find-start p-to-e2 (* 2 e))
                                      (* 2 e))]
            [else (find-power p-to-e e)])))
  (: find-power : Z Z -> Z)
  (define (find-power p-to-e e)
    ;(display (list 'fp 'p-to-e p-to-e  'e e)) (newline)
    ; p-to-e <= n < (square p-to-e)
    (+ e (max-dividing-power-naive p (quotient n p-to-e))))
  (cond [(= p 1)              1]
        [(not (divides? p n)) 0]
        [else                 (assert (find-start p 1) natural?)]))

(: max-dividing-power-naive : Z Z -> N)
(define (max-dividing-power-naive p n)
  ; sames as max-dividing-power but using naive algorithm
  (: loop : Z Z -> Z)
  (define (loop p-to-e e)
    (if (divides? p-to-e n)
        (loop (* p p-to-e) (+ e 1))
        (- e 1)))
  (if (= p 1)
      (error 'max-dividing-power "No maximal power of 1 exists")
      (assert (loop 1 0) natural?)))

; THEOREM (The Chinese Remainder Theorem)
;   Let n1,...,nk be positive integers with gcd(ni,nj)=1 whenever i<>j,
;   and let a1,...,ak be any integers. Then the solutions to
;     x=a1  mod n1,  ...,  x=ak  mod nk
;   has a single solution in {0,...,n-1}, where n=n1*...nk.

; Example : (solve-chinese '(2 3 2) '(3 5 7)) = 23

(: solve-chinese : Zs (Listof N+) -> N)
(define (solve-chinese as ns)
  ; the ns should be coprime
  (let* ([n  (apply * ns)]
         [cs (map (λ: ([ni : Z]) (quotient n ni)) ns)]
         [ds (map modular-inverse cs ns)]
         [es (cast ds integers?)])
    (cast (modulo (apply + (map * as cs es)) n) natural?)))

;;;
;;; PRIMES
;;;

(: odd-prime? : Natural -> Boolean)
(define (odd-prime? n)
  (and (odd? n) (prime? n)))

;;; PRIMALITY TESTS

; Strong pseudoprimality test
; The strong test returns one of:
;   'probably-prime                                        if n is a prime
;   'composite            (with at least probability 1/2)  if n is a composite non-Carmichael number
;   a proper divisor of n (with at least probability 1/2)  if n is a Carmichael number
; [MCA, p.509 - Algorithm 18.5]
(: prime-strong-pseudo-single? : Integer -> (U 'probably-prime 'composite N))
(define (prime-strong-pseudo-single? n)
  (cond
    [(n . <= . 0)  (raise-argument-error 'prime-strong-pseudo-single? "Positive-Integer" n)]
    [(n . >= . 4)
     (define a (random-integer 2 (- n 1)))
     (define g (gcd a n))
     (cond
       [(> g 1) g] ; factor found
       [else
        ; 3. write n-1 = 2^ν * m , m odd
        (let loop ([ν 0] [m (- n 1)])
          (cond 
            [(even? m) (loop (add1 ν) (quotient m 2))]
            [else ; 4. for i=1,...,ν do bi <- b_{i-1}^2 rem N
             (define b (modular-expt a m n))
             (cond 
               [(= b 1) 'probably-prime]
               [else    
                (let loop ([i 0] [b b] [b-old b])
                  (if (and (< i ν) (not (= b 1)))
                      (loop (add1 i)
                            (modulo (* b b) n)
                            b)
                      (if (= b 1)
                          (let ([g (gcd (+ b-old 1) n)])
                            (if (or (= g 1) (= g n))
                                'probably-prime
                                g))
                          'composite)))])]))])]
    [(= n 1)  'composite]
    [else  'probably-prime]))

(define-type Strong-Test-Result         (U 'very-probably-prime 'composite N))

(: prime-strong-pseudo/explanation : N -> Strong-Test-Result)
(define (prime-strong-pseudo/explanation n)
  ; run the strong test several times to improve probability
  (: loop : Z (U Strong-Test-Result 'probably-prime) -> Strong-Test-Result)
  (define (loop trials result)
    (cond [(= trials 0)                 'very-probably-prime]
          [(eq? result 'probably-prime) (loop (sub1 trials) (prime-strong-pseudo-single? n))]
          [else                         result]))
  (loop prime-strong-pseudo-trials (prime-strong-pseudo-single? n)))

(: prime-strong-pseudo? : N -> Boolean)
(define (prime-strong-pseudo? n)
  (let ([explanation (prime-strong-pseudo/explanation n)])
    (or (eq? explanation 'very-probably-prime)
        (eq? explanation #t))))


(: prime? : Integer -> Boolean)
(define prime?
  (let ()
    ; TODO: Only store odd integers in this table
    (define N *SMALL-PRIME-LIMIT*)
    (define ps (make-vector (+ N 1) #t))
    (define ! vector-set!)
    (! ps 0 #f)
    (! ps 1 #f)
    (for ([n (in-range 2 (+ N 1))])
      (when (vector-ref ps n)
        (for ([m (in-range (+ n n) (+ N 1) n)])
          (! ps m #f))))
    (lambda (n)
      (let ([n (abs n)])
        (if (< n N)
            (vector-ref ps n)
            (prime-strong-pseudo? n))))))


(: next-prime : (case-> (N -> N) (Z -> Z)) )
(define (next-prime n)
  (cond
    [(negative? n) (- (prev-prime (abs n)))]
    [(= n 0) 2]
    [(= n 1) 2]
    [(= n 2) 3]
    [(even? n) (let ([n+1 (add1 n)])
                 (if (prime? n+1)
                     n+1
                     (next-prime n+1)))]
    [else      (let ([n+2 (+ n 2)])
                 (if (prime? n+2)
                     n+2
                     (next-prime n+2)))]))

(: untyped-next-prime : Z -> Z)
(define (untyped-next-prime z)
  (next-prime z))

(: untyped-prev-prime : Z -> Z)
(define (untyped-prev-prime z)
  (prev-prime z))


(: prev-prime : Z -> Z)
(define (prev-prime n)
  (cond
    [(negative? n) (- (next-prime (abs n)))]
    [(= n 3)   2]
    [(< n 3)   -2]
    [(even? n) (let ([n-1 (sub1 n)])
                 (if (prime? n-1)
                     n-1
                     (prev-prime n-1)))]
    [else      (let ([n-2 (- n 2)])
                 (if (prime? n-2)
                     n-2
                     (prev-prime n-2)))]))


(: next-primes : Z N -> Zs)
(define (next-primes m primes-wanted)
  (: loop : Z Z -> Zs)
  (define (loop n primes-wanted)
    (if (= primes-wanted 0)
        '()
        (let ([next (next-prime n)])
          (if next
              (cons next (loop next (sub1 primes-wanted)))
              '()))))
  (loop m primes-wanted))

(: prev-primes : Z N -> Zs)
(define (prev-primes m primes-wanted)
  (: loop : Z Z -> Zs)
  (define (loop n primes-wanted)
    (if (= primes-wanted 0)
        '()
        (let ([prev (prev-prime n)])
          (if prev
              (cons prev (loop prev (sub1 primes-wanted)))
              '()))))
  (loop m primes-wanted))


(: nth-prime : N -> Prime)
(define (nth-prime n)
  (for/fold: ([p : Prime  2]) ([m (in-range n)])
    (next-prime p)))


;;;
;;; FACTORIZATION
;;;

(: factorize : N -> (Listof (List N N)))
(define (factorize n)
  (if (< n *SMALL-PRIME-LIMIT*)   ; NOTE: Do measurement of best cut
      (factorize-small n)
      (factorize-large n)))

(: defactorize : (Listof (List N N)) -> N)
(define (defactorize bes)
  (cond [(empty? bes) 1]
        [else (define be (first bes))
              (* (expt (first be) (second be))
                 (defactorize (rest bes)))]))

(: factorize-small : N -> (Listof (List N N)))
(define (factorize-small n)
  ; fast for small n, but works correctly for large n too
  (small-prime-factors-over n 2))

(: small-prime-factors-over : N Prime -> (Listof (List N N)))
; Factor a number n without prime factors below the prime p.
(define (small-prime-factors-over n p) ; p prime
  (cond
    [(< n p)         '()]
    [(= n p)         (list (list p 1))]
    [(prime? n)      (list (list n 1))]
    [(divides? p n)  (let ([m (max-dividing-power p n)])
                       (cons (list p m)
                             (small-prime-factors-over 
                              (quotient n (expt p m))
                              (next-prime p))))]
    [else            (small-prime-factors-over n (next-prime p))]))


;;; ALGORITHM 19.8  Pollard's rho method
; INPUT   n>=3 neither a prime nor a perfect power
; OUTPUT  Either a proper divisor of n or #f
(: pollard : N -> (U N False))
(define (pollard n)
  (let ([x0 (random-natural n)])
    (do ([xi x0 (remainder (+ (* xi xi) 1) n)]
         [yi x0 (remainder (+ (sqr (+ (* yi yi) 1)) 1) n)]
         [i  0  (add1 i)]
         [g  1  (gcd (- xi yi) n)])
      [(or (< 1 g n) (> i (sqrt n)))
       (if (< 1 g n)
           (cast g natural?)
           #f)])))

(: pollard-factorize : Natural -> (Listof (List Natural Natural)))
(define (pollard-factorize n)
  (if (< n *SMALL-FACORIZATION-LIMIT*)
      (factorize-small n)
      (cond
        [(= n 1)        '()]
        [(prime? n)     `((, n 1))]
        [(even? n)      `((2 1) ,@(pollard-factorize (quotient n 2)))]
        [(divides? 3 n) `((3 1) ,@(pollard-factorize (quotient n 3)))]
        [(simple-perfect-power n)
         => (λ: ([base-and-exp : (List N N)]) 
              (cond
                [(prime? (car base-and-exp)) (list base-and-exp)]
                [else (map (λ: ([b-and-e : (List Natural Natural)])
                             (list (car b-and-e) 
                                   (* (cadr base-and-exp) (cadr b-and-e))))
                           (pollard-factorize (car base-and-exp)))]))]
        [else                 
         (let loop ([divisor (pollard n)])
           (if divisor
               (append (pollard-factorize divisor)
                       (pollard-factorize (quotient n divisor)))
               (loop (pollard n))))])))

(: factorize-large : N -> (Listof (List N N)))
(define (factorize-large n)
  (combine-same-base
   (sort (pollard-factorize n) base-and-exponent<?)))

(: base-and-exponent<? : (U N (List N N)) (U N (List N N)) -> Boolean)
(define (base-and-exponent<? x y)
  (let ([id-or-first 
         (λ: ([x : (U Integer (List Integer Integer))])
           (if (number? x) x (first x)))])
    (<= (id-or-first x) (id-or-first y))))

(: combine-same-base : (Listof (List N N)) -> (Listof (List N N)))
(define (combine-same-base list-of-base-and-exponents)
  ; list-of-base-and-exponents must be sorted
  (let ([l list-of-base-and-exponents])
    (cond
      [(null? l)        '()]
      [(null? (cdr l))  l]
      [else             
       (define b1 (first  (first l)))
       (define e1 (second (first l)))
       (define b2 (first  (second l)))
       (define e2 (second (second l)))
       (define more (cddr l))
       (if (= b1 b2)
           (combine-same-base (cons (list b1 (+ e1 e2))
                                    (cdr (cdr list-of-base-and-exponents))))
           (cons (car list-of-base-and-exponents)
                 (combine-same-base (cdr list-of-base-and-exponents))))])))


; find-tail pred clist -> pair or false
; Return the first pair of clist whose car satisfies pred. If no pair does, return false.
(: find-tail : (Z -> Boolean) Zs -> (U False Zs))
(define (find-tail pred xs)
  (cond [(empty? xs) #f]
        [(pred (car xs)) xs]
        [else (find-tail pred (cdr xs))]))


;;;
;;; Powers
;;;

(: as-power : N+ -> (Values N N))
;   Write a>0 as b^r with r maximal. Return b and r.
(define (as-power a)    
  (let ([r (apply gcd ((inst map N (List N N)) second (factorize a)))])
    (values (integer-root a r) r)))


(: prime-power : N -> (U (List Prime N) False))
;   if n is a prime power, return list of prime and exponent in question,
;   otherwise return #f
(define (prime-power n)
  (let ([factorization (prime-divisors/exponents n)])
    (if (= (length factorization) 1)
        (first (prime-divisors/exponents n))
        #f)))

(: prime-power? : N -> Boolean)
;   Is n of the form p^m, with p is prime?
(define (prime-power? n)
  (and (prime-power n) #t))

(: odd-prime-power? : N -> Boolean)
(define (odd-prime-power? n)
  (let ([p/e (prime-power n)])
    (and p/e
         (odd? (first p/e)))))

(: perfect-power? : N -> Boolean)
(define (perfect-power? a)
  (and (not (zero? a))
       (let-values ([(base n) (as-power a)])
         (and (> n 1) (> a 1)))))

(: simple-perfect-power : N -> (U (List N N) False))
(define (simple-perfect-power a)
  ; simple-perfect-power is used by pollard-fatorize
  (and (not (zero? a))
       (let-values ([(base n) (simple-as-power a)])
         (if (and (> n 1) (> a 1))
             (list base n)
             #f))))

(: perfect-power : N -> (U (List N N) False))
;   if a = b^n with b>1 and n>1
(define (perfect-power a)
  (and (not (zero? a))
       (let-values ([(base n) (as-power a)])
         (if (and (> n 1) (> a 1))
             (list base n)
             #f))))

(: perfect-square : N -> (U N False))
(define (perfect-square n)
  (let ([sqrt-n (integer-sqrt n)])
    (if (= (* sqrt-n sqrt-n) n)
        sqrt-n
        #f)))

(: powers-of : N N -> (Listof N))
;   returns a list of numbers: a^0, ..., a^n
(define (powers-of a n)
  (let: loop : (Listof N)
    ([i   : N 0] 
     [a^i : N 1])
    (if (<= i n)
        (cons a^i (loop (+ i 1) (* a^i a)))
        '())))

(define prime-divisors/exponents factorize)

(: prime-divisors : N -> (Listof Prime))
;   return list of primes in a factorization of n
(define (prime-divisors n)
  (map (inst car N (Listof N))
       (prime-divisors/exponents n)))

(: prime-exponents : N -> (Listof N))
;   return list of exponents in a factorization of n
(define (prime-exponents n)
  (map (inst cadr N N (Listof N)) 
       (prime-divisors/exponents n)))


(: integer-root/remainder : N N -> (Values N N))
(define (integer-root/remainder a n)
  (let ([i (integer-root a n)])
    (values i (assert (- a (expt i n)) natural?))))

(: integer-root : N N -> N)
(define (integer-root x y)
    ; y'th root of x
    (cond 
      [(eq? x 0) 0]
      [(eq? x 1) 1]
      [(eq? y 1) x]
      [(eq? y 2) (integer-sqrt x)]
      [(not (integer? y))
       (error 'integer-root "internal error (used to return 1 here - why?) remove after testing")]
      [else
       (define length (integer-length x))
       ;; (expt 2 (- length l 1)) <= x < (expt 2 length)
       (assert
        (cond [(<= length y) 1]
              ;; result is >= 2
              [(<= length (* 2 y))
               ;; result is < 4
               (if (< x (expt 3 y)) 2 3)]
              [(even? y) (integer-root (integer-sqrt x) (quotient y 2))]
              [else
               (let* ([length/y/2 ;; length/y/2 >= 1 because (< (* 2 y) length)
                       (quotient (quotient (- length 1) y) 2)])
                 (let ([init-g
                        (let* ([top-bits          (arithmetic-shift x (- (* length/y/2 y)))]
                               [nth-root-top-bits (integer-root top-bits y)])
                          (arithmetic-shift (+ nth-root-top-bits 1) length/y/2))])
                   (let: loop : Z ([g : Z init-g])
                     (let* ([a (expt g (assert (- y 1) natural?))]
                            [b (* a y)]
                            [c (* a (- y 1))]
                            [d (quotient (+ x (* g c)) b)])
                       (let ([diff (- d g)])
                         (cond [(not (negative? diff))
                                g]
                               [(< diff -1)
                                (loop d)]
                               [else
                                ;; once the difference is one, it's more
                                ;; efficient to just decrement until g^y <= x
                                (let loop ((g d))
                                  (if (not (< x (expt g y)))
                                      g
                                      (loop (- g 1))))]))))))])
        natural?)]))


(: simple-as-power : N+ -> (Values N N))
;    For a>0 write it as a = b^r where r maximal
;    return (values b r)
(define (simple-as-power a)
  ; (displayln (list 'simple-as-power a))
  ; Note: The simple version is used by pollard-factorize
  (let: loop : (Values N N)
    ([n : N (integer-length a)])
    (let-values ([(root rem) (integer-root/remainder a (add1 n))])
      (if (zero? rem)
          (values root (assert (add1 n) natural?))
          (if (positive? n)
              (loop (sub1 n))
              (error 'simple-as-power "internal error"))))))

(: prime-power? : N -> Boolean)

;;;
;;; DIVISORS
;;;

(: divisors : Z -> (Listof N))
;   return the positive divisorts of n
(define (divisors n)
  (cond [(zero? n) '()]
        [else (define n+ (if (positive? n) n (- n)))
              (sort (factorization->divisors (factorize n+)) <)]))

(: factorization->divisors : (Listof (List N N)) -> (Listof N))
(define (factorization->divisors f)
  (cond
    [(null? f) '(1)]
    [else (let ([p (first (first f))]
                [n (second (first f))]
                [g (rest f)])
            ; f = p^n * g
            (let ([divisors-of-g (factorization->divisors g)])
              (apply append
                     ((inst map (Listof N) N)
                      (λ: ([p^i : N]) (map (λ: ([d : N]) (* p^i d)) divisors-of-g))
                      (powers-of p n)))))]))

;;;
;;; Number theoretic functions
;;;

; DEFINITION (Euler's phi function  aka  totient)
;  phi(n) is the number of integers a=1,2,... such that gcd(a,n)=1

; THEOREM
;   If m and n are coprime then
;     phi(mn) = phi(m) phi(n) 

; THEOREM (Euler's phi function)
;  If the prime power factorization of p is
;           e1     ek
;     n = p1 ... pk     , where pi is prime and ei>0
;  then
;                   k          1
;   phi(n) = n * product (1 - ---- )
;                  i=1         pi

(: totient : N -> N)
(define (totient n)
  (let ((ps (prime-divisors n)))
    (assert (* (quotient n (apply * ps))
               (apply * (map (λ: ([p : N]) (sub1 p)) ps)))
            natural?)))

(: every : (All (A) (A -> Boolean) (Listof A) -> Boolean))
(define (every pred xs)
  (or (empty? xs)
      (and (pred (car xs))
           (every pred (cdr xs)))))


; moebius-mu : natural -> {-1,0-1}
;   mu(n) =  1  if n is a product of an even number of primes
;         = -1  if n is a product of an odd number of primes
;         =  0  if n has a multiple prime factor
(: moebius-mu : N -> (U -1 0 1))
(define (moebius-mu n)
  (: one? : Z -> Boolean)
  (define (one? x) (= x 1))
  (define f         (factorize n))
  (define exponents ((inst map N (List N N)) second f))
  (cond 
    [(every one? exponents)
     (define primes ((inst map N (List N N)) first f))
     (if (even? (length primes))
         1 -1)]
    [else 0]))


(: divisor-sum : (case-> (N -> N) (N N -> N)))                   
(define divisor-sum 
  ; returns the sum of the kth power of all divisors of n
  (let ()
    (case-lambda 
      [(n)   (divisor-sum n 1)]
      [(n k) (let* ([f  (factorize n)]
                    [ps ((inst map N (List N N)) first f)]
                    [es ((inst map N (List N N)) second f)])
               (: divisor-sum0 : Any N -> N)
               (define (divisor-sum0 p e) (+ e 1))
               (: divisor-sum1 : N N -> N)
               (define (divisor-sum1 p e)
                 (let: loop : N 
                   ([sum    : N 1]
                    [n      : N 0]
                    [p-to-n : N 1])
                   (cond [(= n e) sum]
                         [else (let ([t (* p p-to-n)])
                                 (loop (+ t sum) (+ n 1) t))])))
               (: divisor-sumk : N N -> N)
               (define (divisor-sumk p e)
                 (let ([p-to-k (expt p k)])
                   (let: loop : N
                     ([sum     : N 1]
                      [n       : N 0]
                      [p-to-kn : N 1])
                     (cond [(= n e) sum]
                           [else (let ([t (* p-to-k p-to-kn)])
                                   (loop (+ t sum) (+ n 1) t))]))))
               (cast
                (apply * (map (cond [(= k 0) divisor-sum0]
                                    [(= k 1) divisor-sum1]
                                    [else    divisor-sumk])
                              ps es))
                natural?))])))


; These tests are for un-exported functions.
#;(begin
    (require typed/rackunit)
    
    (check-equal? (max-dividing-power-naive 3 27) 3)
    (check-equal? (max-dividing-power-naive 3 (* 27 2)) 3)
    
    (check-true   (<= 4 (random-integer 4 5) 4))
    
    (check-false (prime-fermat? 0))
    (check-false (prime-fermat? 1))
    (check-false (prime-fermat? 4))
    (check-false (prime-fermat? 6))
    (check-false (prime-fermat? 8))
    
    (check-equal? (prime-fermat? 2)   #t)
    (check-equal? (prime-fermat? 3)   #t)
    (check-equal? (prime-fermat? 5)   'possibly-prime)
    (check-equal? (prime-fermat? 7)   'possibly-prime)
    (check-equal? (prime-fermat? 11)  'possibly-prime)
    (check-true   (member? (prime-fermat? 561) '(#f possibly-prime))) ; Carmichael number
    
    (check-equal? (prime-strong-pseudo-single? 4) 2)
    (check-true  (member? (prime-strong-pseudo-single? 6) '(2 3)))
    (check-true  (member? (prime-strong-pseudo-single? 8) '(2 4 composite)))
    
    (check-equal? (prime-strong-pseudo-single? 5)   'probably-prime)
    (check-equal? (prime-strong-pseudo-single? 7)   'probably-prime)
    (check-equal? (prime-strong-pseudo-single? 11)  'probably-prime)
    ;; Carmichael number:
    (check-true   (member? (prime-strong-pseudo-single? 561) (cons 'probably-prime (divisors 561))))
    )