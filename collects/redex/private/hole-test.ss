#lang scheme
(require redex)

(define-language tl-grammar
  [v (cont (hide-hole E))]
  [E hole
     (v ... E)])

(define the-test
  (reduction-relation
   tl-grammar
   [--> (in-hole E_1 (explode))
        (in-hole E_1 1)]))

(test--> the-test 
         (term ((cont hole) (explode)))
         (term ((cont hole) 1)))