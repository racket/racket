#lang racket
(require redex)

(define-language λv
  (e (e e ...) (if0 e e e) (+ e e) x v)
  (v (λ ((x t) ...) e) number)
  (t (-> t ... t) num)
  (E (v ... E e ...) (if0 E e e) (+ E e) (+ v E) hole)
  (x variable-not-otherwise-mentioned))

(define red
  (reduction-relation
   λv
   (--> (in-hole E (+ number_1 number_2))
        (in-hole E ,(+ (term number_1) 
                       (term number_2)))
        "+")
   (--> (in-hole E (if0 0 e_1 e_2))
        (in-hole E e_1)
        "if0t")
   (--> (in-hole E (if0 number_1 e_1 e_2))
        (in-hole E e_2)
        "if0f"
        (side-condition 
          (not (= 0 (term number_1)))))
   (--> (in-hole E ((λ ((x t) ..._1) e) v ..._1))
        (in-hole E (subst-n (x v) ... e))
        "βv")))

(define-metafunction λv
  subst-n : (x any) ... any -> any
  [(subst-n (x_1 any_1) (x_2 any_2) ... any_3)
   (subst x_1 any_1 (subst-n (x_2 any_2) ... 
                             any_3))]
  [(subst-n any_3) any_3])

(define-metafunction λv
  subst : x any any -> any
  ;; 1. x_1 bound, so don't continue in λ body
  [(subst x_1 any_1 (λ ((x_2 t_2) ... (x_1 t_1) (x_3 t_3) ...) any_2))
   (λ ((x_2 t_2) ... (x_1 t_1) (x_3 t_3) ...) any_2)]
  ;; 2. general purpose capture avoiding case
  [(subst x_1 any_1 (λ ((x_2 t_2) ...) any_2))
   (λ ((x_new t_2) ...) 
     (subst x_1 any_1
            (subst-vars (x_2 x_new) ... 
                        any_2)))
   (where (x_new ...)
          ,(variables-not-in
            (term (x_1 any_1 any_2)) 
            (term (x_2 ...))))]
  ;; 3. replace x_1 with e_1
  [(subst x_1 any_1 x_1) any_1]
  ;; 4. x_1 and x_2 are different, so don't replace
  [(subst x_1 any_1 x_2) x_2]
  ;; the last cases cover all other expressions
  [(subst x_1 any_1 (any_2 ...))
   ((subst x_1 any_1 any_2) ...)]
  [(subst x_1 any_1 any_2) any_2])

(define-metafunction λv
  subst-vars : (x any) ... any -> any
  [(subst-vars (x_1 any_1) x_1) any_1]
  [(subst-vars (x_1 any_1) (any_2 ...)) 
   ((subst-vars (x_1 any_1) any_2) ...)]
  [(subst-vars (x_1 any_1) any_2) any_2]
  [(subst-vars (x_1 any_1) (x_2 any_2) ... any_3) 
   (subst-vars (x_1 any_1) 
               (subst-vars (x_2 any_2) ... any_3))]
  [(subst-vars any) any])

(define-metafunction λv
  tc : e (x t) ... -> t or #f
  [(tc number (x t) ...)
   num]
  [(tc (+ e_1 e_2) (x t) ...) 
   num
   (where num (tc e_1 (x t) ...))
   (where num (tc e_2 (x t) ...))]
  [(tc (if0 e_1 e_2 e_3) (x t) ...)
   t_2
   (where num (tc e_1 (x t) ...))
   (where t_2 (tc e_2 (x t) ...))
   (where t_2 (tc e_3 (x t) ...))]
  [(tc x_1 (x_2 t_2) ... (x_1 t_1) (x_3 t_3) ...)
   t_1
   (side-condition (not (member (term x_1) (term (x_2 ...)))))]
  [(tc (e_1 e_2 ...) (x t) ...)
   t_3
   (where (-> t_2 ... t_3) (tc e_1 (x t) ...))
   (where (t_2 ...) ((tc e_2 (x t) ...) ...))]
  [(tc (λ ((x_1 t_1) ...) e) (x_2 t_2) ...)
   (-> t_1 ... t)
   (where t (tc e (x_1 t_1) ... (x_2 t_2) ...))]
  [(tc e (x t) ...) #f])

;; remove the #; to run an example
#;
(traces red
        (term 
         (+ ((λ ((n num)) 
               (if0 n
                    1
                    0))
             (+ 2 2))
            2)))

(test-equal (term (subst x y x)) (term y))
(test-equal (term (subst x y z)) (term z))
(test-equal (term (subst x y (x (y z)))) (term (y (y z))))
(test-equal (term (subst x y ((λ ((x num)) x) ((λ ((y1 num)) y1) (λ ((x num)) z)))))
            (term ((λ ((x num)) x) ((λ ((y2 num)) y2) (λ ((x num)) z)))))
(test-equal (term (subst x y (if0 (+ 1 x) x x)))
            (term (if0 (+ 1 y) y y)))
(test-equal (term (subst x (λ ((z num)) y) (λ ((y num)) x)))
            (term (λ ((y1 num)) (λ ((z num)) y))))
(test-equal (term (subst x 1 (λ ((y num)) x)))
            (term (λ ((y num)) 1)))
(test-equal (term (subst x y (λ ((y num)) x)))
            (term (λ ((y1 num)) y)))
(test-equal (term (subst x (λ ((y num)) y) (λ ((z num)) (z2 z))))
            (term (λ ((z1 num)) (z2 z1))))
(test-equal (term (subst x (λ ((z num)) z) (λ ((z num)) (z1 z))))
            (term (λ ((z2 num)) (z1 z2))))
(test-equal (term (subst x z (λ ((z num)) (z1 z))))
            (term (λ ((z2 num)) (z1 z2))))
(test-equal (term (subst x3 5 (λ ((x2 num)) x2)))
            (term (λ ((x1 num)) x1)))
(test-equal (term (subst z * (λ ((z num) (x num)) 1)))
            (term (λ ((z num) (x num)) 1)))
(test-equal (term (subst q (λ ((x num)) z) (λ ((z num) (x num)) q)))
            (term (λ ((z1 num) (x1 num)) (λ ((x num)) z))))
(test-equal (term (subst x 1 (λ ((x num) (x num)) x)))
            (term (λ ((x num) (x num)) x)))

(test-->> red (term ((λ ((x num)) x) 1)) 1)
(test-->> red (term ((λ ((x num) (y num)) x) 1 2)) 1)
(test-->> red (term ((λ ((x num) (y num)) y) 1 2)) 2)
(test-->> red (term (((λ ((x num)) (λ ((x num)) x)) 1) 2)) 2)
(test-->> red (term (((λ ((x num)) (λ ((y num)) x)) 1) 2)) 1)
(test-->> red (term ((λ ((x num)) (+ x x)) 2)) 4)
(test-->> red (term ((λ ((x num)) (if0 x x (+ x 1))) 2)) 3)
(test-->> red (term ((λ ((x num)) (if0 x x (+ x 1))) 0)) 0)
(test-->> red 
          (term (((λ ((x num)) (λ ((y num) (z num)) x)) 1) 2))
          (term ((λ ((y num) (z num)) 1) 2)))
(test-->> red
          (term (+ (+ 1 2) (+ 3 4)))
          (term 10))

(test-equal (term (tc 1)) (term num))
(test-equal (term (tc (1 1))) (term #f))
(test-equal (term (tc x (x num))) (term num))
(test-equal (term (tc x)) (term #f))
(test-equal (term (tc x (x num) (x (-> num num)))) (term num))
(test-equal (term (tc ((λ ((x num)) x) 1))) (term num))
(test-equal (term (tc ((λ ((f (-> num num)) (x num)) (f x)) (λ ((x num)) x) 1))) (term num))
(test-equal (term (tc (+ (+ 1 2) 3))) (term num))
(test-equal (term (tc (if0 1 (λ ((x num)) x) 3))) (term #f))
(test-equal (term (tc (if0 1 2 3))) (term num))
(test-equal (term (tc (λ ((x num)) (x)))) (term #f))

(test-results)
