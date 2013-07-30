#lang typed/racket

;; Make sure that recursive types defined using
;; different recursion methods are compatible with
;; each other

(define-type (L1 A) (U Null (Pairof A (L1 A))))
(define-type (L2 A) (Rec X (U Null (Pairof A X))))
(define-type (L3 A) (Rec X (U Null (Pairof A (L3 A)))))

(: x (L1 Integer))
(define x '(1 2 3))

(: y (L2 Number))
(define y x)

(: z (L3 Number))
(define z x)

(: a (Listof Number))
(define a x)

(: b (L1 Number))
(define b a)

(: c (L1 Number))
(define c y)

(: d (L1 Number))
(define d z)

