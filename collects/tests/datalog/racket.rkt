#lang racket
(require datalog tests/eli-tester)

(define parent (make-theory))

(test
 (datalog parent
          (! (parent joseph2 joseph1))
          (! (parent joseph2 lucy))
          (! (parent joseph3 joseph2)))
 =>
 empty
 
 (datalog parent
          (? (parent X joseph2)))
 =>
 (list '(parent joseph3 joseph2))
 
 (datalog parent
          (? (parent joseph2 X)))
 =>
 (list '(parent joseph2 joseph1)
       '(parent joseph2 lucy))
 
 (datalog parent
          (? (parent joseph2 X))
          (? (parent X joseph2)))
 =>
 (list '(parent joseph3 joseph2))
 
 (datalog parent
          (! (:- (ancestor A B)
                 (parent A B)))
          (! (:- (ancestor A B)
                 (parent A C)
                 (= D C) ; Unification required
                 (ancestor D B))))
 =>
 empty
 
 (datalog parent
          (? (ancestor A B)))
 =>
 (list '(ancestor joseph3 joseph2)
       '(ancestor joseph2 lucy)
       '(ancestor joseph2 joseph1)
       '(ancestor joseph3 lucy)
       '(ancestor joseph3 joseph1))
 
 (let ([x 'joseph2])
   (datalog parent
            (? (parent x X))))
 =>
 (list '(parent joseph2 joseph1)
       '(parent joseph2 lucy))
 
 (datalog parent
          (? (add1 1 :- X)))
 =>
 (list '(add1 1 :- 2))
 
 )