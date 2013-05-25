#lang racket
(require redex
         "subst.rkt")

(reduction-steps-cutoff 10)

(define-language lang
  (e (e e)
     x
     number
     (lambda (x t) e)
     (if e e e)
     (= e e)
     (-> e e)
     num
     bool)
  (c (t c)
     (c e)
     (-> t c)
     (-> c e)
     (= t c)
     (= c e)
     (if c e e)
     (if t c e)
     (if t t c)
     hole)
  (x (variable-except lambda -> if =))
  (t num bool (-> t t)))

(define reductions
  (reduction-relation
   lang
   (r--> number num)
   
   (r--> (lambda (x_1 t_1) e_body)
         (-> t_1 (subst (x_1 t_1 e_body))))
   
   (r--> ((-> t_1 t_2) t_1) t_2)
   
   (e--> (side-condition ((-> t_1 t) t_2)
                         (not (equal? (term t_1) (term t_2))))
         ,(format "app: domain error ~s and ~s" (term t_1) (term t_2)))
   
   (e--> (num t_1)
         ,(format "app: non function error ~s" (term t_1)))
   
   (r--> (if bool t_1 t_1) t_1)
   (e--> (side-condition (if bool t_1 t_2)
                         (not (equal? (term t_1) (term t_2))))
         ,(format "if: then and else clause mismatch ~s and ~s" (term t_1) (term t_2)))
   (e--> (side-condition (if t_1 t t)
                         (not (equal? (term t_1) 'bool)))
         ,(format "if: test not boolean ~s" (term t_1)))
   
   (r--> (= num num) bool)
   (e--> (side-condition (= t_1 t_2)
                         (or (not (equal? (term t_1) 'num))
                             (not (equal? (term t_2) 'num))))
         ,(format "=: not comparing numbers ~s and ~s" (term t_1) (term t_2)))
   
   with
   
   [(--> (in-hole c_1 a) (in-hole c_1 b)) (r--> a b)]
   [(--> (in-hole c a) b) (e--> a b)]))

(traces reductions
        '((lambda (x num) (lambda (y num) (if (= x y) 0 x))) 1))
(traces reductions
        '((lambda (x num) (lambda (y num) (if (= x y) 0 (lambda (x num) x)))) 1))
