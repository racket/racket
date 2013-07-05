#lang racket

#|

A core contract calculus, including blame, 
with function contracts, (eager) pair contracts, 
and a few numeric predicates

|#

(require redex/reduction-semantics
         redex/examples/subst)

(define-language lang
  (e (e e ...)
     x
     number
     (λ (x ...) e)
     
     (if e e e)
     #t #f
     
     cons car cdr
     
     -> or/c
     ac
     pred?
     (blame l)
     l)
  (pred? number?
         odd?
         positive?)
  (E (v ... E e ...)
     (if E e e)
     hole)
  (v number
     (λ (x ...) e)
     cons car cdr
     (cons v v)
     pred?
     -> or/c ac
     (-> v ...)
     (or/c v ...)
     #t #f
     l)

  (l + -) ;; blame labels
  
  (x variable-not-otherwise-mentioned))

(define reds
  (reduction-relation
   lang
   (--> (in-hole E ((λ (x ...) e) v ...))
        (in-hole E (subst-n ((x v) ... e)))
        (side-condition (= (length (term (x ...)))
                           (length (term (v ...)))))
        βv)
   
   (--> (in-hole E (if #t e_1 e_2)) (in-hole E e_1) ift)
   (--> (in-hole E (if #f e_1 e_2)) (in-hole E e_2) iff)
   
   (--> (in-hole E (number? number)) (in-hole E #t))
   (--> (in-hole E (number? v))
        (in-hole E #f)
        (side-condition (not (number? (term v)))))

   (--> (in-hole E (car (cons v_1 v_2))) 
        (in-hole E v_1))
   (--> (in-hole E (cdr (cons v_1 v_2))) 
        (in-hole E v_2))
   
   (--> (in-hole E (odd? number))
        (in-hole E #t)
        (side-condition (odd? (term number))))
   (--> (in-hole E (odd? v))
        (in-hole E #f)
        (side-condition (or (not (number? (term v)))
                            (not (odd? (term v))))))
   
   (--> (in-hole E (positive? number))
        (in-hole E #t)
        (side-condition (positive? (term number))))
   (--> (in-hole E (positive? v))
        (in-hole E #f)
        (side-condition (or (not (number? (term v)))
                            (not (positive? (term v))))))

   
   (--> (in-hole E (blame l)) 
        (blame l)
        (side-condition (not (equal? (term E) (term hole)))))
   
   (--> (in-hole E (ac pred? v l))
        (in-hole E (if (pred? v) v (blame l))))
   (--> (in-hole E (ac (-> v_dom ... v_rng) (λ (x ...) e) l))
        (in-hole E (λ (x ...) (ac v_rng ((λ (x ...) e) (ac v_dom x l_2) ...) l)))
        (where l_2 (¬ l)))
   
   (--> (in-hole E (ac (cons v_1 v_2) (cons v_3 v_4) l))
        (in-hole E (cons (ac v_1 v_3 l) (ac v_2 v_4 l))))
   
   (--> (in-hole E (ac (or/c pred? v_1 v_2 ...) v_3 l))
        (in-hole E (if (pred? v_3) 
                       v_3
                       (ac (or/c v_1 v_2 ...) v_3 l))))
   (--> (in-hole E (ac (or/c v_1) v_2 l))
        (in-hole E (ac v_1 v_2 l)))
   ))

(define-metafunction lang
  [(¬ +) -]
  [(¬ -) +])

(test-->> reds (term ((λ (x y) x) 1 2)) 1)
(test-->> reds (term ((λ (x y) y) 1 2)) 2)
(test-->> reds (term (if (if #t #f #t) #f #t)) (term #t))
(test-->> reds (term (positive? 1)) #t)
(test-->> reds (term (positive? -1)) #f)
(test-->> reds (term (positive? (λ (x) x))) #f)
(test-->> reds (term (odd? 1)) #t)
(test-->> reds (term (odd? 2)) #f)
(test-->> reds (term (odd? (λ (x) x))) #f)
(test-->> reds (term (car (cdr (cdr (cons 1 (cons 2 (cons 3 #f))))))) 3)

(test-->> reds (term ((λ (x) x) (blame -))) (term (blame -)))
(test-->> reds (term (ac number? 1 +)) 1)
(test-->> reds (term (ac number? (λ (x) x) +)) (term (blame +)))
(test-->> reds (term ((ac (-> number? number?) (λ (x) x) +) 1)) 1)
(test-->> reds 
         (term ((ac (-> number? number?) (λ (x) x) +) #f)) 
         (term (blame -)))
(test-->> reds 
         (term ((ac (-> number? number?) (λ (x) #f) +) 1))
         (term (blame +)))
(test-->> reds
         (term (ac (or/c odd? positive?) 1 +))
         1)
(test-->> reds
         (term (ac (or/c odd? positive?) -1 +))
         -1)
(test-->> reds
         (term (ac (or/c odd? positive?) 2 +))
         2)
(test-->> reds
         (term (ac (or/c odd? positive?) -2 +))
         (term (blame +)))

(test-->> reds
         (term (ac (cons odd? positive?) (cons 3 1) +))
         (term (cons 3 1)))

(test-results)
