#lang scheme
(require redex "subst.ss")

(reduction-steps-cutoff 10)

(define-language lang
  (e (e e)
     (abort e)
     x
     v)
  (c (v c)
     (c e)
     hole)
  (v call/cc
     number
     (lambda (x) e))
  
  (x (variable-except lambda call/cc abort)))

(define reductions
  (reduction-relation
   lang
   (--> (in-hole c_1 (call/cc v_arg))
        ,(term-let ([v (variable-not-in (term c_1) 'x)])
           (term (in-hole c_1 (v_arg (lambda (v) (abort (in-hole c_1 v)))))))
        call/cc)
   (--> (in-hole c (abort e_1)) 
        e_1
        abort)
   (--> (in-hole c_1 ((lambda (variable_x) e_body) v_arg))
        (in-hole c_1 (subst (variable_x v_arg e_body)))
        βv)))

(traces reductions '((lambda (x) (x x)) (lambda (x) (x x))))
(traces reductions '((call/cc call/cc) (call/cc call/cc)))
(traces reductions '((lambda (x) ((call/cc call/cc) x)) (call/cc call/cc)))
