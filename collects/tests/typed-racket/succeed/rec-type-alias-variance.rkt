#lang typed/racket

;; Test subtyping and variance for recursive type aliases

(define-type (B X)
  (U (Listof X) (Setof (B X))))

(: b1 (B String))
(define b1 (list "foo"))

(: b2 (B (U String Symbol)))
(define b2 b1)

(: f (All (Y) (Y -> (B Y))))
(define (f x) (list x))

(: g (All (X) (X -> (B X))))
(define g f)

;; Adapted from struct variance test
(define-type (Boxer D) (List (Boxer2 D)))
(define-type (Boxer2 D) (List (D -> Void) (Boxer D)))

(: f-Boxer (All (D) ((Boxer D) D -> Void)))
(define (f-Boxer boxer v)
  ((car (car boxer)) v))

;; The last line in this example would error without
;; registering the variance
(define-type (Even A) (U Null (Pairof A (Odd A))))
(define-type (Odd A) (Pairof A (Even A)))

(: even->odd (All (A) (A (Even A) -> (Odd A))))
(define (even->odd elem lst)
  (cons elem lst))

(even->odd 3 '(1 2))

