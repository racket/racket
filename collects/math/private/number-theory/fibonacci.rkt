#lang typed/racket/base

(provide make-fibonacci
         fibonacci
         make-fibonacci/mod
         fibonacci/mod)

(: generator : Integer Integer Natural Natural Natural -> Integer)
(define (generator a b p q count)
  ; SICP ex. 1.19
  (cond 
    [(zero? count) b]
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

(: make-fibonacci (Integer Integer -> (Integer -> Integer)))
(define ((make-fibonacci a b) n)
  (cond [(n . < . 0)  (raise-argument-error 'fibonacci "Natural" n)]
        [else  (generator b a 0 1 n)]))

(define fibonacci (make-fibonacci 0 1))

(: generator/mod : Integer Integer Natural Natural Natural Positive-Integer -> Integer)
(define (generator/mod a b p q count mod)
  (cond 
    [(zero? count) (modulo b mod)]
    [(even? count)
     (generator/mod a b
                    (modulo (+ (* p p) (* q q)) mod)
                    (modulo (+ (* 2 p q) (* q q)) mod)
                    (quotient count 2)
                    mod)]
    [else
     (generator/mod (modulo (+ (* b q) (* a q) (* a p)) mod)
                    (modulo (+ (* b p) (* a q)) mod)
                    p
                    q
                    (- count 1)
                    mod)]))

(: make-fibonacci/mod (Integer Integer -> (Integer Integer -> Integer)))
(define ((make-fibonacci/mod a b) n mod)
  (cond [(n . < . 0)  (raise-argument-error 'fibonacci "Natural" 0 n mod)]
        [(mod . <= . 0)  (raise-argument-error 'fibonacci "Positive-Integer" 1 n mod)]
        [else  (generator/mod b a 0 1 n mod)]))

(define fibonacci/mod (make-fibonacci/mod 0 1))
