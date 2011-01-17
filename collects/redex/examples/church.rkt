#lang racket
(require redex)

(reduction-steps-cutoff 100)

(define-language lang
  (e (lambda (x) e)
     (let (x e) e)
     (app e e)
     (+ e e)
     number
     x)
  (e-ctxt (lambda (x) e-ctxt)
          a-ctxt)
  (a-ctxt (let (x a-ctxt) e)
          (app a-ctxt e)
          (app x a-ctxt)
          hole)
  (v (lambda (x) e)
     x)
  (x variable))

(define reductions
  (reduction-relation
   lang
   (--> (in-hole e-ctxt_1 (app (lambda (x_1) e_body) e_arg))
        (in-hole e-ctxt_1 (subst (x_1 e_arg e_body))))
   (--> (in-hole e-ctxt_1 (let (x_1 v_1) e_1))
        (in-hole e-ctxt_1 (subst (x_1 v_1 e_1))))))

(define-metafunction lang
  [(subst (x_1 e_1 (lambda (x_1) e_2))) (lambda (x_1) e_2)]
  [(subst (x_1 e_1 (lambda (x_2) e_2))) 
   ,(term-let ((x_new (variable-not-in (term e_1) (term x_2))))
      (term (lambda (x_new) (subst (x_1 e_1 (subst (x_2 x_new e_2)))))))]
  [(subst (x_1 e_1 (let (x_1 e_2) e_3))) (let (x_1 (subst (x_1 e_1 e_2))) e_3)]
  [(subst (x_1 e_1 (let (x_2 e_2) e_3)))
   ,(term-let ((x_new (variable-not-in (term e_1) (term x_2))))
      (term (let (x_2 (subst (x_1 e_1 e_2))) (subst (x_1 e_1 (subst (x_2 x_new e_3)))))))]
  [(subst (x_1 e_1 x_1)) e_1]
  [(subst (x_1 e_1 x_2)) x_2]
  [(subst (x_1 e_1 (app e_2 e_3)))
   (app (subst (x_1 e_1 e_2))
        (subst (x_1 e_1 e_3)))]
  [(subst (x_1 e_1 (+ e_2 e_3)))
   (+ (subst (x_1 e_1 e_2))
      (subst (x_1 e_1 e_3)))]
  [(subst (x_1 e_1 number_1)) number_1])

(traces reductions
        '(let (plus (lambda (m) 
                      (lambda (n) 
                        (lambda (s)
                          (lambda (z)
                            (app (app m s) (app (app n s) z)))))))
           (let (two (lambda (s) (lambda (z) (app s (app s z)))))
             (app (app plus two) two))))
