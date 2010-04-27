#lang scheme
(require redex)

(define-language tl-grammar
  [v (cont (hide-hole E))]
  [E hole
     (v ... E)])

(define test1
  (reduction-relation
   tl-grammar
   [--> (in-hole E_1 (explode))
        (in-hole E_1 1)]))

(test--> test1 
         (term ((cont hole) (explode)))
         (term ((cont hole) 1)))

(define test2
  (reduction-relation
   tl-grammar
   [--> (in-hole E_1 (explode))
        (asplode E_1)]))

(define-metafunction tl-grammar
  asplode : E -> any
  [(asplode ((cont hole) hole))
   okay])

(test--> test2
         (term ((cont hole) (explode)))
         (term okay))

(test-results)
