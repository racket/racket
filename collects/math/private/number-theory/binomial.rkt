#lang typed/racket
(provide binomial)

(define-predicate natural? Natural)

(: binomial : Natural Natural -> Natural)
(define (binomial n k)
  ;  compute the binomial coeffecient n choose k
  ;  http://www.gnu.org/software/gmp/manual/html_node/Binomial-Coefficients-Algorithm.html
  (cond
    [(= k 0)       1]    
    [(= k 1)       n]
    [(> k n)       0]
    [(= k 2)       (assert (quotient (* n (- n 1)) 2) natural?)]
    [(> k (/ n 2)) (binomial n (assert (- n k) natural?))]
    [else          (assert (* (+ n (- k) 1)
                              (let ()
                                (define: prod : Exact-Rational 1)
                                (for: ([i : Natural (in-range 2 (+ k 1))])
                                  (set! prod (* prod (/ (+ n (- k) i) i))))
                                prod))
                           natural?)]))
