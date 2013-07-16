#lang racket

(require (except-in racklog %append))

;A list of trivial programs in Prolog, just so you can get used
;to racklog syntax.

;(%length l n) holds if length(l) = n

(define %length
  (%rel (h t n m)
    (('() 0))
    (((cons h t) n) (%length t m) (%is n (+ m 1)))))

;(%delete x y z) holds if z is y with all x's removed

(define %delete
  (%rel (x y z w)
    ((x '() '()))
    ((x (cons x w) y) (%delete x w y))
    ((x (cons z w) (cons z y)) (%not (%= x z)) (%delete x w y))))

;(%remdup x y) holds if y is x without duplicates

(define %remdup
  (%rel (x y z w)
    (('() '()))
    (((cons x y) (cons x z)) (%delete x y w) (%remdup w z))))

;(%count x n) holds if n is the number of elements in x without
;counting duplicates

(define %count*
  (%rel (x n y)
    ((x n) (%remdup x y) (%length y n))))

;same thing

(define %count
  (letrec ((countaux
	     (%rel (m n m+1 x y z)
	       (('() m m))
	       (((cons x y) m n)
		(%delete x y z) (%is m+1 (+ m 1)) (countaux z m+1 n)))))
    (%rel (x n)
      ((x n) (countaux x 0 n)))))

;(%append x y z) holds if z is the concatenation of x and y

(define %append
  (%rel (x y z w)
    (('() x x))
    (((cons x y) z (cons x w)) (%append y z w))))

;(%reverse x y) holds if the y is the reversal of x

(define %reverse*
  (%rel (x y z yy)
    (('() '()))
    (((cons x y) z) (%reverse y yy) (%append yy (list x) z))))

;same thing, but tailcall optimizing

(define %reverse
  (letrec ((revaux
	     (%rel (x y z w)
	       (('() y y))
	       (((cons x y) z w) (revaux y (cons x z) w)))))
    (%rel (x y)
      ((x y) (revaux x '() y)))))

;(%fact n m) holds if m = n!

(define %fact*
  (%rel (n n! n-1 n-1!)
    ((0 1))
    ((n n!) (%is n-1 (- n 1)) (%fact n-1 n-1!) (%is n! (* n n-1!)))))

;same thing, but tailcall optimizing

(define %fact
  (letrec ((factaux
	     (%rel (n! m x m-1 xx)
	       ((0 n! n!))
	       ((m x n!) (%is m-1 (- m 1)) (%is xx (* x m))
		(factaux m-1 xx n!)))))
    (%rel (n n!)
      ((n n!) (factaux n 1 n!)))))
