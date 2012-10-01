#lang typed/racket/base

(require "../number-theory/binomial.rkt"
         "../unsafe.rkt"
         "../exception.rkt")

(provide factorial binomial permutations multinomial)

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
(define simple-cutoff 244)

(: factorial-simple (Nonnegative-Fixnum -> Positive-Integer))
(define (factorial-simple n)
  (cond [(n . < . fact-table-size)  (vector-ref fact-table n)]
        [else  (* n (factorial-simple (- n 1)))]))

(: factorial (case-> (Negative-Integer -> Nothing)
                     (Integer -> Positive-Integer)))
(define (factorial n)
  (cond [(not (nonnegative-fixnum? n))  (raise-argument-error 'factorial "Nonnegative-Fixnum" n)]
        [(n . < . simple-cutoff)  (factorial-simple n)]
        [else
         (let: loop : Positive-Integer ([n : Positive-Fixnum  n]
                                        [m : Positive-Fixnum  1])
           (define n-m (- n m))
           (cond [(n-m . <= . 0)  n]
                 [else  (define 2m (unsafe-fx* m 2))
                        (* (loop n 2m) (loop n-m 2m))]))]))

(: permutations (case-> (Negative-Integer Integer -> Nothing)
                        (Integer Negative-Integer -> Nothing)
                        (Integer Integer -> Natural)))
(define (permutations n k)
  (cond [(not (nonnegative-fixnum? n))
         (raise-argument-error 'permutations "Nonnegative-Fixnum" 0 n k)]
        [(not (nonnegative-fixnum? k))
         (raise-argument-error 'permutations "Nonnegative-Fixnum" 1 n k)]
        [(zero? k)  1]
        [(k . > . n)  0]  ; also handles n = 0 case
        [else  (define m (/ (factorial n) (factorial (- n k))))
               (with-asserts ([m  exact-nonnegative-integer?]) m)]))

(: multinomial (Integer Integer * -> Natural))
(define (multinomial n . ks)
  (cond [(not (nonnegative-fixnum? n))
         (raise-argument-error 'multinomial "Nonnegative-Fixnum" 0 n ks)]
        [(not (andmap nonnegative-fixnum? ks))
         (raise-argument-error 'multinomial "(Listof Nonnegative-Fixnum)" 1 n ks)]
        [(not (= n (apply + ks)))  0]
        [else  (define m (apply / (factorial n) (map factorial ks)))
               (with-asserts ([m  exact-nonnegative-integer?]) m)]))
