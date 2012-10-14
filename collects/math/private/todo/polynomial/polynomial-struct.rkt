#lang typed/racket/base

(require racket/list
         racket/match
         "basis-index.rkt")

(struct: (A) polynomial-term ([coefficient : A] [basis : Basis-Index])
  #:transparent)

(define-type (Polynomial-Terms A) (Listof (polynomial-term A)))

(struct: (A) Polynomial ([terms : (Polynomial-Terms A)])
  #:transparent)

(: sort-polynomial-terms (All (A) ((Polynomial-Terms A) -> (Polynomial-Terms A))))
(define (sort-polynomial-terms ts)
  ((inst sort (polynomial-term A) Basis-Index)
    ts
    basis-index<
    #:key polynomial-term-basis
    #:cache-keys? #t))

(: make-polynomial (All (A) ((Polynomial-Terms A) -> (Polynomial A))))
(define (make-polynomial ts)
  (Polynomial (sort-polynomial-terms ts)))

(: poly+ (All (A) ((Polynomial A) (Polynomial A) (A A -> A) -> (Polynomial A))))
(define (poly+ p0 p1 +)
  (define v0s (Polynomial-terms p0))
  (define v1s (Polynomial-terms p1))
  (Polynomial
   (let: loop : (Polynomial-Terms A) ([v0s : (Polynomial-Terms A)  (Polynomial-terms p0)]
                                      [v1s : (Polynomial-Terms A)  (Polynomial-terms p1)])
     (cond [(empty? v0s)  v1s]
           [(empty? v1s)  v0s]
           [else
            (define v0 (first v0s))
            (define v1 (first v1s))
            (define i0 (polynomial-term-basis v0))
            (define i1 (polynomial-term-basis v1))
            (cond [(basis-index< i0 i1)  (cons v0 (loop (rest v0s) v1s))]
                  [(basis-index< i1 i0)  (cons v1 (loop v0s (rest v1s)))]
                  [else  (list* (polynomial-term
                                 (+ (polynomial-term-coefficient v0)
                                    (polynomial-term-coefficient v1)) i0)
                                (loop (rest v0s) (rest v1s)))])]))))

(: term-cons (All (A) ((polynomial-term A) (Polynomial-Terms A) (A A -> A) -> (Polynomial-Terms A))))
(define (term-cons t ts +)
  (define i (polynomial-term-basis t))
  (let: loop : (Polynomial-Terms A) ([ts : (Polynomial-Terms A)  ts])
    (cond [(empty? ts)  (list t)]
          [else
           (define t0 (first ts))
           (define i0 (polynomial-term-basis t0))
           (cond [(basis-index< i i0)  (cons t ts)]
                 [(basis-index< i0 i)  (list* t0 (loop (rest ts)))]
                 [else  (list* (polynomial-term
                                (+ (polynomial-term-coefficient t)
                                   (polynomial-term-coefficient t0)) i)
                               (rest ts))])])))

(: poly* (All (A) ((Polynomial A) (Polynomial A) (A A -> A) (A A -> A) -> (Polynomial A))))
(define (poly* p0 p1 + *)
  (define v0s (Polynomial-terms p0))
  (define v1s (Polynomial-terms p1))
  (Polynomial
   (let: loop0 : (Polynomial-Terms A) ([v0s : (Polynomial-Terms A)  (Polynomial-terms p0)])
     (cond [(empty? v0s)  empty]
           [else
            (match-define (polynomial-term c0 i0) (first v0s))
            (let: loop1 : (Polynomial-Terms A) ([v1s : (Polynomial-Terms A)  (Polynomial-terms p1)])
              (cond [(empty? v1s)  (loop0 (rest v0s))]
                    [else
                     (match-define (polynomial-term c1 i1) (first v1s))
                     (define new-t (polynomial-term (* c0 c1) (basis-index+ i0 i1)))
                     (define new-ts (loop1 (rest v1s)))
                     (term-cons new-t new-ts +)]))]))))
