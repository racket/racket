#lang typed/racket/base

(require racket/unsafe/ops
         racket/math racket/flonum
         "utils.rkt")

(provide factorial binomial permutations multinomial)

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

(: factorial (Integer -> Positive-Integer))
(define (factorial n)
  (cond [(not (nonnegative-fixnum? n))  (raise-type-error 'factorial "Nonnegative-Fixnum" n)]
        [(n . < . simple-cutoff)  (factorial-simple n)]
        [else
         (let: loop : Positive-Integer ([n : Positive-Fixnum  n]
                                        [m : Positive-Fixnum  1])
           (define n-m (- n m))
           (cond [(n-m . <= . 0)  n]
                 [else  (define 2m (unsafe-fx* m 2))
                        (* (loop n 2m) (loop n-m 2m))]))]))

(: binomial (Integer Integer -> Natural))
(define (binomial n k)
  (cond [(not (nonnegative-fixnum? n))  (raise-type-error 'binomial "Nonnegative-Fixnum" 0 n k)]
        [(not (nonnegative-fixnum? k))  (raise-type-error 'binomial "Nonnegative-Fixnum" 1 n k)]
        [(k . > . n)  0]
        [(zero? k)  1]
        [(zero? n)  0]
        [else  (define m (/ (factorial n) (factorial k) (factorial (- n k))))
               (with-asserts ([m  exact-nonnegative-integer?]) m)]))

(: permutations (Integer Integer -> Natural))
(define (permutations n k)
  (cond [(not (nonnegative-fixnum? n))  (raise-type-error 'permutations "Nonnegative-Fixnum" 0 n k)]
        [(not (nonnegative-fixnum? k))  (raise-type-error 'permutations "Nonnegative-Fixnum" 1 n k)]
        [(k . > . n)  0]
        [(zero? k)  1]
        [(zero? n)  0]
        [else  (define m (/ (factorial n) (factorial (- n k))))
               (with-asserts ([m  exact-nonnegative-integer?]) m)]))

(: multinomial (Integer Integer * -> Natural))
(define (multinomial n . ks)
  (cond [(not (nonnegative-fixnum? n))  (raise-type-error 'multinomial "Nonnegative-Fixnum" 0 n ks)]
        [(not (listof-nonnegative-fixnum? ks))
         (raise-type-error 'multinomial "(Listof Nonnegative-Fixnum)" 1 n ks)]
        [(not (= n (apply + ks)))  (error 'multinomial "expected ks to sum to n; given ~e and ~e"
                                          n ks)]
        [else  (define m (apply / (factorial n) (map factorial ks)))
               (with-asserts ([m  exact-nonnegative-integer?]) m)]))

(module* test typed/racket
  (require (submod "..") typed/rackunit)
  
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
  
  (check-equal? (multinomial 20 3 4 5 8) 3491888400))
