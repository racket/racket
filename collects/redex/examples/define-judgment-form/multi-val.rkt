#lang racket

(require redex/reduction-semantics)

(define-language funny-pairs
  (e (e e ...)
     x
     v)
  (v (λ (x ...) e)
     (pair v v)
     pair
     b
     number)
  (b fst
     snd
     one)
  (E hole
     (v ... E e ...))
  (x variable-not-otherwise-mentioned))

(define reduction
  (reduction-relation
   funny-pairs #:domain e
   (--> (in-hole E ((λ (x ..._1) e) v ..._1))
        (in-hole E (subst e [x v] ...)))
   (--> (in-hole E (b v_1))
        (in-hole E v_2)
        (judgment-holds (δ b v_1 v_2)))))

(define-judgment-form funny-pairs
  #:mode (δ I I O)
  [(δ fst (pair v_1 v_2) v_1)]
  [(δ snd (pair v_1 v_2) v_2)]
  [(δ one (pair v_1 v_2) v_1)]
  [(δ one (pair v_1 v_2) v_2)])

(define-metafunction funny-pairs
  subst : e [x v] ... -> e
  [(subst e) e]
  [(subst e [x_1 v_1] [x_2 v_2] ...)
   (subst (subst1 e x_1 v_1) [x_2 v_2] ...)])

(define-metafunction funny-pairs
  subst1 : e x v -> e
  [(subst1 (e ...) x v)
   ((subst1 e x v) ...)]
  [(subst1 x x v) v]
  [(subst1 x_1 x_2 v) x_1]
  [(subst1 (λ (x_1 ... x_i x_i+1 ...) e) x_i v)
   (λ (x_1 ... x_i x_i+1 ...) e)]
  [(subst1 (λ (x_1 ...) e) x v)
   ; capture shmapture...
   (λ (x_1 ...) (subst1 e x v))]
  [(subst1 pair x v) pair]
  [(subst1 b x v) b])

(test-->> reduction
          (term (fst (pair 1 2)))
          (term 1))
(test-->> reduction
          (term (snd (pair 1 2)))
          (term 2))
(test-->> reduction
          (term (snd (λ (x) x)))
          (term (snd (λ (x) x))))
(test-->> reduction
          (term (one (pair 1 2)))
          (term 1)
          (term 2))
(test-->> reduction
          (term ((one (pair fst snd))
                 ((one (pair fst snd))
                  (pair (pair 1 2) (pair 3 4)))))
          (term 1)
          (term 2)
          (term 3)
          (term 4))
(test-->> reduction
          (term ((λ (x) x) 1))
          (term 1))
(test-->> reduction
          (term ((λ (x y) y) 1 2))
          (term 2))
(test-->> reduction
          (term (((λ (x) (λ (x) x)) 1) 2))
          (term 2))
(test-->> reduction
          (term (((λ (x) (λ (y) (x y))) (λ (z) z)) 1))
          (term 1))
