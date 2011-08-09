#lang racket

(require redex)

(define-language non-det-arith
  (e (e e)
     x
     v)
  (v (λ (x) e)
     integer
     add1)
  (E hole
     (E e)
     (v E))
  (x variable-not-otherwise-mentioned))

(define reduction
  (reduction-relation
   non-det-arith
   (--> (in-hole E ((λ (x) e) v))
        (in-hole E (subst e x v)))
   (--> (in-hole E (add1 integer))
        (in-hole E v)
        (judgment-holds (δ add1 integer v)))))

(define-judgment-form non-det-arith
  #:mode (δ I I O)
  [(δ add1 integer 0)]
  [(δ add1 integer v)
   (where v (Σ 1 integer))])

(define-metafunction non-det-arith
  Σ : integer ... -> integer
  [(Σ integer ...)
   ,(apply + (term (integer ...)))])

(define-metafunction non-det-arith
  subst : e x v -> e
  [(subst (e_1 e_2) x v)
   ((subst e_1 x v) (subst e_2 x v))]
  [(subst x x v) v]
  [(subst x_1 x_2 v) x_1]
  [(subst (λ (x) e) x v)
   (λ (x) e)]
  [(subst (λ (x_1) e) x_2 v)
   ; capture shmapture...
   (λ (x_1) (subst e x_2 v))]
  [(subst integer x v) integer]
  [(subst add1 x v) add1])

(traces reduction (term (((λ (x) (λ (x) (x (add1 7)))) (add1 0)) add1)))