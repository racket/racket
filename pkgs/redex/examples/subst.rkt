#lang racket
(require redex/reduction-semantics)
(provide subst subst-n)

(define-language subst-lang
  (x variable))

(define-metafunction subst-lang
  [(subst-n ((x_1 any_1) (x_2 any_2) ... any_3))
   (subst (x_1 any_1 (subst-n ((x_2 any_2) ... any_3))))]
  [(subst-n (any_3)) any_3])

(define-metafunction subst-lang
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst (x_1 any_1 (λ (x_2 ... x_1 x_3 ...) any_2)))
   (λ (x_2 ... x_1 x_3 ...) any_2)
   (side-condition (not (member (term x_1) (term (x_2 ...)))))]
  ;; 2. general purpose capture avoiding case
  [(subst (x_1 any_1 (λ (x_2 ...) any_2)))
   ,(term-let ([(x_new ...)
                (variables-not-in (term (x_1 any_1 any_2)) 
                                  (term (x_2 ...)))])
              (term 
               (λ (x_new ...) 
                 (subst (x_1 any_1 (subst-vars ((x_2 x_new) ... any_2)))))))]
  ;; 3. replace x_1 with e_1
  [(subst (x_1 any_1 x_1)) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst (x_1 any_1 x_2)) x_2]
  ;; the last two cases cover all other expression forms
  [(subst (x_1 any_1 (any_2 ...)))
   ((subst (x_1 any_1 any_2)) ...)]
  [(subst (x_1 any_1 any_2)) any_2])

(define-metafunction subst-lang
  [(subst-vars ((x_1 any_1) x_1)) any_1]
  [(subst-vars ((x_1 any_1) (any_2 ...))) ((subst-vars ((x_1 any_1) any_2)) ...)]
  [(subst-vars ((x_1 any_1) any_2)) any_2]
  [(subst-vars ((x_1 any_1) (x_2 any_2) ... any_3)) 
   (subst-vars ((x_1 any_1) (subst-vars ((x_2 any_2) ... any_3))))]
  [(subst-vars (any)) any])

(begin
  (test-equal (term (subst (x y x))) (term y))
  (test-equal (term (subst (x y z))) (term z))
  (test-equal (term (subst (x y (x (y z))))) (term (y (y z))))
  (test-equal (term (subst (x y ((λ (x) x) ((λ (y1) y1) (λ (x) z))))))
        (term ((λ (x) x) ((λ (y2) y2) (λ (x) z)))))
  (test-equal (term (subst (x y (if0 (+ 1 x) x x))))
        (term (if0 (+ 1 y) y y)))
  (test-equal (term (subst (x (λ (z) y) (λ (y) x))))
        (term (λ (y1) (λ (z) y))))
  (test-equal (term (subst (x 1 (λ (y) x))))
        (term (λ (y) 1)))
  (test-equal (term (subst (x y (λ (y) x))))
        (term (λ (y1) y)))
  (test-equal (term (subst (x (λ (y) y) (λ (z) (z2 z)))))
        (term (λ (z1) (z2 z1))))
  (test-equal (term (subst (x (λ (z) z) (λ (z) (z1 z)))))
        (term (λ (z2) (z1 z2))))
  (test-equal (term (subst (x z (λ (z) (z1 z)))))
        (term (λ (z2) (z1 z2))))
  (test-equal (term (subst (x3 5 (λ (x2) x2))))
        (term (λ (x1) x1)))
  (test-equal (term (subst (z * (λ (z x) 1))))
        (term (λ (z x) 1)))
  (test-equal (term (subst (q (λ (x) z) (λ (z x) q))))
        (term (λ (z1 x1) (λ (x) z))))
  (test-equal (term (subst (x 1 (λ (x x) x))))
        (term (λ (x x) x)))
  (test-results))
