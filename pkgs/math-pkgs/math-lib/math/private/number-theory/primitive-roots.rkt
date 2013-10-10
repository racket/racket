#lang typed/racket/base

(require racket/list
         "divisibility.rkt"
         "modular-arithmetic.rkt"
         (only-in "number-theory.rkt" 
                  odd-prime-power?
                  totient
                  prime-divisors
                  prime?
                  odd-prime?
                  prime-power))

(provide unit-group
         unit-group-order
         unit-group-orders
         exists-primitive-root?
         primitive-root?
         primitive-root
         primitive-roots)

; DEFINITION (Order)
;  If G is a finite group with identity element e,
;  then the order of g in G is the least k>0 such that
;  g^k=e

; DEFINITION (Un)
;  The group of units in Zn with respect to multiplication
;  modulo n is called Un.

(: unit-group : Integer -> (Listof Positive-Integer))
(define (unit-group n)
  (cond [(n . <= . 0)  (raise-argument-error 'unit-group "Positive-Integer" n)]
        [else  (filter (λ: ([m : Natural]) (coprime? m n))
                       (build-list (- n 1) add1))]))

(: unit-group-order : Integer Integer -> Positive-Integer)
(define (unit-group-order g n)
  (cond [(g . <= . 0)  (raise-argument-error 'unit-group-order "Positive-Integer" 0 g n)]
        [(n . <= . 0)  (raise-argument-error 'unit-group-order "Positive-Integer" 1 g n)]
        [(not (coprime? g n))
         (error 'unit-group-order "expected coprime arguments; given ~e and ~e" g n)]
        [else
         (with-modulus n
           (let: loop : Positive-Integer ([k : Positive-Integer 1] 
                                          [a : Natural g])
             (cond [(mod= a 1)  k]
                   [else  (loop (+ k 1) (mod* a g))])))]))

(: unit-group-orders : Integer -> (Listof Positive-Integer))
(define (unit-group-orders n)
  (cond [(n . <= . 0)  (raise-argument-error 'unit-group-orders "Positive-Integer" n)]
        [else  (map (λ: ([m : Positive-Integer]) (unit-group-order m n))
                    (unit-group n))]))

; DEFINITION (Primitive Root)
;  A generator g of Un is called a primitive root mod n.
;  I.e.  order(g)=phi(n)  <=>  g is primitive

#;
(define (primitive-root? g n)
  (if (not (coprime? g n))
      (error 'primitive-root? "expected coprime arguments; given ~e and ~e" g n)
      (= (unit-group-order g n) (phi n))))

; THEOREM (Existence of primitive roots)
;      Un is cyclic   (i.e. have a primitive root)
;  <=> n = 1, 2, 4, p^e, 2*p^e  where p is an odd prime

(: exists-primitive-root? : Integer -> Boolean)
(define (exists-primitive-root? n)
  (cond [(n . <= . 0)  (raise-argument-error 'exists-primitive-root? "Positive-Integer" n)]
        [(or (= n 1) (= n 2) (= n 4))  #t]
        [(odd? n)  (odd-prime-power? n)]
        [else      (odd-prime-power? (quotient n 2))]))

; LEMMA
;       a in Un is a primitive root
;  <=>   phi(n)/q
;       a         <> 1  in Un for all primes q dividing phi(n)

(: primitive-root? : Integer Integer -> Boolean)
(define (primitive-root? g n)
  (cond [(g . <= . 0)  (raise-argument-error 'primitive-root? "Positive-Integer" 0 g n)]
        [(n . <= . 0)  (raise-argument-error 'primitive-root? "Positive-Integer" 1 g n)]
        [(not (coprime? g n))
         (error 'primitive-root? "expected coprime arguments; given ~e and ~e" g n)]
        [else
         (define phi-n (totient n))
         (with-modulus n
           (andmap (λ: ([x : Boolean]) x)
                   (map (λ: ([q : Natural]) (not (mod= (modexpt g (quotient phi-n q)) 1)))
                        (prime-divisors phi-n))))]))

; primitive-root : N -> Un
;  return primitive root of n if one exists,
;  otherwise return #f
#;
(define (primitive-root n)
  (and (exists-primitive-root? n)
       (let* ([phi-n (phi n)]
              [qs    (prime-divisors phi-n)])
         (define (primitive-root? g)
           (with-modulus n (andmap (lambda (x) x)
                                   (map (lambda (q)
                                          (not (= (expt g (/ phi-n q)) 1)))
                                        qs))))
         (let loop ([g 1])
           (cond
             [(= g n)                #f]
             [(not (coprime? g n))   (loop (+ g 1))]
             [(primitive-root? g)    g]
             [else                   (loop (+ g 1))])))))

; LEMMA
;  If Un has a primitive root, then it has phi(phi(n)) primitive roots

; primitive-roots : integer -> list
;  return list of all primitive roots of Un
(: primitive-roots : Integer -> (Listof Natural))
(define (primitive-roots n)
  (cond [(n . <= . 0)  (raise-argument-error 'primitive-roots "Positive-Integer" n)]
        [(not (exists-primitive-root? n))  empty]
        [else
         (let* ([phi-n (totient n)]
                [qs    (prime-divisors phi-n)])
           (: primitive-root? : Natural -> Boolean)
           (define (primitive-root? g)
             (with-modulus n
               (andmap (λ: ([x : Boolean]) x)
                       (map (λ: ([q : Natural])
                              (not (mod= (modexpt g (quotient phi-n q)) 1)))
                            qs))))
           (let: loop : (Listof Natural)
             ([g     : Natural          1] 
              [roots : (Listof Natural) empty])
             (cond
               [(= g n)                (reverse roots)]
               [(not (coprime? g n))   (loop (+ g 1)  roots)]
               [(primitive-root? g)    (loop (+ g 1) (cons g roots))]
               [else                   (loop (+ g 1)  roots)])))]))

(: primitive-root : Integer -> (U Natural False))
(define (primitive-root n)
  (cond [(n . <= . 0)  (raise-argument-error 'primitive-root "Positive-Integer" n)]
        [(not (exists-primitive-root? n))  #f]
        ; U_p^e , p odd
        [(and (odd-prime-power? n) (not (prime? n)))
         (define pp (prime-power n))
         (define p (if pp (first pp) (error 'primitive-root "internal error")))
         (define gg (primitive-root p))
         (define g (or gg (error 'primitive-root "internal error")))
         (if (= (unit-group-order g (* p p)) (totient (* p p)))
             g
             (modulo (+ g p) n))]
        ; U_2p^e , p odd
        [(and (even? n) (odd-prime? (quotient n 2)))
         (define gg (primitive-root (quotient n 2)))
         (define g (or gg (error 'primitive-root "internal error")))
         (if (odd? g)
             g
             (modulo (+ g (quotient n 2)) n))]
        ; General case
        [else                                
         (define phi-n (totient n))
         (define qs    (prime-divisors phi-n))
         (: primitive-root? : Natural -> Boolean)
         (define (primitive-root? g)
           (with-modulus n
             (andmap (λ: ([x : Boolean]) x)
                     (map (λ: ([q : Natural])
                            (not (mod= (modexpt g (quotient phi-n q)) 1)))
                          qs))))
         (let: loop : (U Natural False)
           ([g : Natural 1])
           (cond
             [(= g n)                #f]
             [(not (coprime? g n))   (loop (+ g 1))]
             [(primitive-root? g)    g]
             [else                   (loop (+ g 1))]))]))
