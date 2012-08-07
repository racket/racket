#lang typed/racket
(provide farey)

(define-type Q Exact-Rational)
(define-predicate natural? Natural)

(: mediant : Q Q -> Q)
(define (mediant x y)
  (/ (+ (numerator x) (numerator y))
     (+ (denominator x) (denominator y))))

(define memo ((inst make-hasheqv Q Q)))

(: farey : Natural -> (Listof Q))
(define (farey d)
  
  (: delete-last : (Listof Q) -> (Listof Q))
  (define (delete-last xs)
    (reverse (cdr (reverse xs))))
  
  (: successive? : Q Q -> Boolean)
  (define (successive? x y)
    (= (- (* (denominator x) (numerator y))
          (* (denominator y) (numerator x)))
       1))
  
  (cond
    [(= d 0) (raise-type-error 'farey "Expected positive number" d)]
    [(= d 1) '(0 1)]
    [else    
     (define fs (farey (assert (sub1 d) natural?)))
     (sort (append fs
                   (filter (λ: ([x : Q]) (<= (denominator x) d))
                           (map mediant
                                (delete-last fs) (rest fs))))
           <)]))
