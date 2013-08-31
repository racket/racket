#lang typed/racket

(define s (set 0 1 2 3))
(define q (seteq 0 1 2 3))
(define v (seteqv 0 1 2 3))
(define s0 (ann (set) (Setof Byte)))

(set-empty? s)
(set-empty? q)
(set-empty? v)
(set-empty? s0)

(set-count s)
(set-count q)
(set-count v)
(set-count s0)

(set-member? s 0)
(set-member? q 0)
(set-member? v 0)
(set-member? s0 0)

(set-add s 4)
(set-add q 4)
(set-add v 4)
(set-add s0 4)

(set-remove s 4)
(set-remove q 4)
(set-remove v 4)
(set-remove s0 4)

(subset? s s0)
(set-map v add1)
(set-for-each s0 display)

(set-equal? s)
(set-eqv? v)
(set-eq? q)
(cast (and (set? s0) s0) (Setof Any))
(generic-set? s0)
