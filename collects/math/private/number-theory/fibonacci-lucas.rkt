#lang typed/racket

(provide fibonacci 
         fibonacci-mod
         lucas)

(: generator : Natural Natural Natural Natural Integer -> Natural)
(define (generator a b p q count)
  ; SICP ex. 1.19
  (cond 
    [(= count 0) b]
    [(even? count)
     (generator a b
                (+ (* p p) (* q q))
                (+ (* 2 p q) (* q q))
                (quotient count 2))]
    [else
     (generator (+ (* b q) (* a q) (* a p))
                (+ (* b p) (* a q))
                p
                q
                (- count 1))]))

(: fibonacci : Natural -> Natural)
(define (fibonacci n)
  (generator 1 0 0 1 n))

(: lucas : Natural -> Natural)
(define (lucas n)
  (generator 3 1 0 1 n))

(: fibonacci-mod : Natural Natural -> Natural)
; nth Fibonacci number modulo mod
(define (fibonacci-mod n mod)
  (: generator : Natural Natural Natural Natural Integer -> Natural)
  (define (generator a b p q count)
    (cond 
      [(= count 0) b]
      [(even? count)
       (generator a b
                  (remainder (+ (* p p) (* q q)) mod)
                  (remainder (+ (* 2 p q) (* q q)) mod)
                  (quotient count 2))]
      [else
       (generator (remainder (+ (* b q) (* a q) (* a p)) mod)
                  (remainder (+ (* b p) (* a q)) mod)
                  p
                  q
                  (- count 1))]))
  (generator 1 0 0 1 n))
