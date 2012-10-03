#lang typed/racket/base

(require racket/list
         "number-theory.rkt")

(provide quadratic-character quadratic-residue?)

; DEFINITION (Quadratic residue)
;   a in Un is a quadratic residue,
;   if there exists an s such that a=s^2 (mod n)
;   The number s is called a squre root of a modulo n.

; p is prime
(: quadratic-character : Natural Natural -> Integer)
(define (quadratic-character a p)
  (let ([l (with-modulus p (^ a (quotient (- p 1) 2)))])
    (if (<= 0 l 1) l -1)))

(: quadratic-residue? : Natural Natural -> Boolean)
(define (quadratic-residue? a n)
  (let* ([ps     (prime-divisors n)]
         [odd-ps (if (= (first ps) 2)
                     (rest ps)
                     ps)])
    (and (andmap (λ: ([p : Natural]) 
                   (= (quadratic-character a p) 1))
                 odd-ps)
         (cond 
           [(divides? 8 n)  (= (modulo a 8) 1)]
           [(divides? 4 n)  (= (modulo a 4) 1)]
           [else            #t]))))
