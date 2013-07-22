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
 (list (hasheq 'X 'joseph3))
 
 (datalog parent
          (? (parent joseph2 X)))
 =>
 (list (hasheq 'X 'joseph1)
       (hasheq 'X 'lucy))
 
 (datalog parent
          (? (parent joseph2 X))
          (? (parent X joseph2)))
 =>
 (list (hasheq 'X 'joseph3))
 
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
 (list (hasheq 'A 'joseph3 'B 'joseph2)
       (hasheq 'A 'joseph2 'B 'lucy)
       (hasheq 'A 'joseph2 'B 'joseph1)
       (hasheq 'A 'joseph3 'B 'lucy)
       (hasheq 'A 'joseph3 'B 'joseph1))
 
 (let ([x 'joseph2])
   (datalog parent
            (? (parent x X))))
 =>
 (list (hasheq 'X 'joseph1)
       (hasheq 'X 'lucy))
 
 (datalog parent
          (? (add1 1 :- X)))
 =>
 (list (hasheq 'X 2))
 
 (let ()
   (define new-parent
     (with-input-from-bytes 
         (with-output-to-bytes (λ () (write-theory parent)))
       (λ () (read-theory))))
   (test
    (datalog new-parent
             (? (ancestor A B)))
    =>
    (list (hasheq 'A 'joseph3 'B 'joseph2)
          (hasheq 'A 'joseph2 'B 'lucy)
          (hasheq 'A 'joseph2 'B 'joseph1)
          (hasheq 'A 'joseph3 'B 'lucy)
          (hasheq 'A 'joseph3 'B 'joseph1))))
 
 (local [(local-require datalog/tests/examples/ancestor)]
   (datalog theory
            (? (ancestor A B))))
 =>
 (list (hasheq 'A 'ebbon 'B 'bob)
       (hasheq 'A 'bob 'B 'john)
       (hasheq 'A 'john 'B 'douglas)
       (hasheq 'A 'bob 'B 'douglas)
       (hasheq 'A 'ebbon 'B 'john)
       (hasheq 'A 'ebbon 'B 'douglas))
 
  (local [(local-require datalog/tests/paren-examples/ancestor)]
   (datalog theory
            (? (ancestor A B))))
 =>
 (list (hasheq 'A 'ebbon 'B 'bob)
       (hasheq 'A 'bob 'B 'john)
       (hasheq 'A 'john 'B 'douglas)
       (hasheq 'A 'bob 'B 'douglas)
       (hasheq 'A 'ebbon 'B 'john)
       (hasheq 'A 'ebbon 'B 'douglas))
 
 )
