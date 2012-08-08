#lang typed/racket
(provide unit-group
         order
         orders
         exists-primitive-root?
         primitive-root?
         primitive-root
         primitive-roots)
         
(require (only-in "number-theory.rkt" 
                  coprime?
                  with-modulus
                  odd-prime-power?
                  totient
                  prime-divisors
                  prime?
                  odd-prime?
                  prime-power))

; DEFINITION (Order)
;  If G is a finite group with identity element e,
;  then the order of g in G is the least k>0 such that
;  g^k=e

; DEFINITION (Un)
;  The group of units in Zn with respect to multiplication
;  modulo n is called Un.

(: natural-interval : Natural Natural -> (Listof Natural))
(define (natural-interval from to)
  (cond [(= from to) '()]
        [else (cons from (natural-interval (add1 from) to))]))


(: unit-group : Positive-Integer -> (Listof Natural))
(define (unit-group n)
  (when (= n 1) (raise-type-error 'unit-group "expected n>1, got ~a" n))    
  ((inst filter Natural Boolean) 
   (λ: ([m : Natural]) (coprime? m n))
   (natural-interval 1 n)))


(: order : Natural Natural -> Natural)
(define (order g n)
  (if (not (coprime? g n))
      (error "In (order g n) the g and n must me coprime")
      (with-modulus n
                    (let: loop : Natural
                      ([k : Natural 1] 
                       [a : Natural g])
                      (if (= a 1)
                          k
                          (loop (+ k 1) (* a g)))))))

(: orders : Positive-Integer -> (Listof Natural))
(define (orders n)
  (map (λ: ([m : Natural]) (order m n))
       (unit-group n)))

; DEFINITION (Primitive Root)
;  A generator g of Un is called a primitive root mod n.
;  I.e.  order(g)=phi(n)  <=>  g is primitive

#;
(define (primitive-root? g n)
  (if (not (coprime? g n))
      (error "In (primitive-root? g n) the g and n must me coprime")
      (= (order g n) (phi n))))

; THEOREM (Existence of primitive roots)
;      Un is cyclic   (i.e. have a primitive root)
;  <=> n = 1, 2, 4, p^e, 2*p^e  where p is an odd prime

(: exists-primitive-root? : Natural -> Boolean)
(define (exists-primitive-root? n)
  (cond 
    [(member n '(1 2 4)) #t]
    [(odd? n)            (odd-prime-power? n)]
    [else                (odd-prime-power? (quotient n 2))]))


; LEMMA
;       a in Un is a primitive root
;  <=>   phi(n)/q
;       a         <> 1  in Un for all primes q dividing phi(n)

(: primitive-root? : Natural Natural -> Boolean)
(define (primitive-root? g n)
  (unless (coprime? g n)
    (error "In (primitive-root? g n) the g and n must me coprime"))
  (define phi-n (totient n))
  (with-modulus n
                ((inst andmap Boolean Boolean Boolean)
                 (λ: ([x : Boolean]) x)
                 (map (λ: ([q : Natural]) (not (= (^ g (quotient phi-n q)) 1)))
                      (prime-divisors phi-n)))))

; primitive-root : N -> Un
;  return primitive root of n if one exists,
;  otherwise return #f
#;
(define (primitive-root n)
  (and (exists-primitive-root? n)
       (let* ([phi-n (phi n)]
              [qs    (prime-divisors phi-n)])
         (define (primitive-root? g)
           (with-modulus n
                         (andmap (lambda (x) x)
                                 (map (lambda (q)
                                        (not (= (^ g (/ phi-n q)) 1)))
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
(: primitive-roots : Natural -> (Listof Natural))
(define (primitive-roots n)
  (if (not (exists-primitive-root? n))
      '()
      (let* ([phi-n (totient n)]
             [qs    (prime-divisors phi-n)])
        (: primitive-root? : Natural -> Boolean)
        (define (primitive-root? g)
          (with-modulus n
                        ((inst andmap Boolean Boolean Boolean)
                         (λ: ([x : Boolean]) x)
                         (map (λ: ([q : Natural])
                                (not (= (^ g (quotient phi-n q)) 1)))
                              qs))))
        (let: loop : (Listof Natural)
          ([g     : Natural          1] 
           [roots : (Listof Natural) empty])
          (cond
            [(= g n)                (reverse roots)]
            [(not (coprime? g n))   (loop (+ g 1)  roots)]
            [(primitive-root? g)    (loop (+ g 1) (cons g roots))]
            [else                   (loop (+ g 1)  roots)])))))

(: primitive-root : Natural -> (U Natural False))
(define (primitive-root n)
  (and (exists-primitive-root? n)
       (cond
         ; U_p^e , p odd
         [(and (odd-prime-power? n) (not (prime? n)))
          (define pp (prime-power n))
          (define p (if pp (first pp) (error 'primitive-root "internal error")))
          (define gg (primitive-root p))
          (define g (or gg (error 'primitive-root "internal error")))
          (if (= (order g (* p p)) (totient (* p p)))
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
                          ((inst andmap Boolean Boolean Boolean)
                           (λ: ([x : Boolean]) x)
                           (map (λ: ([q : Natural])
                                  (not (= (^ g (quotient phi-n q)) 1)))
                                qs))))
          (let: loop : (U Natural False)
            ([g : Natural 1])
            (cond
              [(= g n)                #f]
              [(not (coprime? g n))   (loop (+ g 1))]
              [(primitive-root? g)    g]
              [else                   (loop (+ g 1))]))])))


