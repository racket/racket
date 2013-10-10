#lang typed/racket/base

(require "../unsafe.rkt")

(provide factorial permutations multinomial)

(define-predicate nonnegative-fixnum? Nonnegative-Fixnum)

;; The number of factorials whose flonum representation is finite
;; Makes generating the flonum table in gamma.rkt fast
(: fact-table-size Positive-Fixnum)
(define fact-table-size 171)

(: fact-table (Vectorof Positive-Integer))
(define fact-table
  (list->vector
   (reverse
    (foldl (λ: ([n  : Positive-Integer]
                [ns : (Listof Positive-Integer)])
             (cons (* n (car ns)) ns))
           '(1)
           (build-list (- fact-table-size 1) add1)))))

(: simple-cutoff Positive-Fixnum)
;; The point at which it seems to be faster to use a more complicated recurrence
(define simple-cutoff 244)

(: factorial-simple (Nonnegative-Fixnum -> Positive-Integer))
(define (factorial-simple n)
  (cond [(n . < . fact-table-size)  (vector-ref fact-table n)]
        [else  (* n (factorial-simple (- n 1)))]))

(: factorial (case-> (Zero -> One)
                     (One -> One)
                     (Integer -> Positive-Integer)))
(define (factorial n)
  (cond [(negative? n)  (raise-argument-error 'factorial "Natural" n)]
        [(not (fixnum? n))  (raise-argument-error 'factorial "Nonnegative-Fixnum" n)]
        [(eqv? n 0)  1]
        [(eqv? n 1)  1]
        [(n . < . simple-cutoff)  (factorial-simple n)]
        [else
         (let: loop : Positive-Integer ([n : Positive-Fixnum  n]
                                        [m : Positive-Fixnum  1])
           (define n-m (- n m))
           (cond [(n-m . <= . 0)  n]
                 [else  (define 2m (unsafe-fx* m 2))
                        (* (loop n 2m) (loop n-m 2m))]))]))

(: permutations (case-> (Integer Zero -> One)
                        (One One -> One)
                        (Integer Integer -> Natural)))
(define (permutations n k)
  (cond [(negative? n)  (raise-argument-error 'permutations "Natural" 0 n k)]
        [(negative? k)  (raise-argument-error 'permutations "Natural" 1 n k)]
        [(zero? k)  1]
        [(k . > . n)  0]
        [else  (assert (/ (factorial n) (factorial (- n k)))
                       exact-nonnegative-integer?)]))

(: multinomial (Integer (Listof Integer) -> Natural))
(define (multinomial n ks)
  (cond [(negative? n)  (raise-argument-error 'multinomial "Natural" 0 n ks)]
        [(ormap negative? ks)  (raise-argument-error 'multinomial "(Listof Natural)" 1 n ks)]
        [(not (= n (apply + ks)))  0]
        [else  (assert (apply / (factorial n) (map factorial ks))
                       exact-nonnegative-integer?)]))
