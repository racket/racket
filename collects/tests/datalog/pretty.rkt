#lang racket
(require rackunit
         datalog/parse
         datalog/pretty)
(provide pretty-tests)

(define pretty-tests
  (test-suite
   "Pretty"
   
   (test-equal? "program"
                (format-program
                 (parse-program
                  (open-input-string #<<END
parent(john, douglas).
parent(john, douglas)?
parent(john, ebbon)?
parent(bob, john).
parent(ebbon, bob).
parent(A, B)?
parent(john, B)?
parent(A, A)?
ancestor(A, B) :- parent(A, B).
ancestor(A, B) :- 
 parent(A, C),
 ancestor(C, B).
ancestor(A, B)?
parent(bob, john)~
parent(A,B)?
ancestor(A,B)?
END
                      )))
                #<<END
parent(john, douglas).
parent(john, douglas)?
parent(john, ebbon)?
parent(bob, john).
parent(ebbon, bob).
parent(A, B)?
parent(john, B)?
parent(A, A)?
ancestor(A, B) :- parent(A, B).
ancestor(A, B) :- parent(A, C), ancestor(C, B).
ancestor(A, B)?
parent(bob, john)~
parent(A, B)?
ancestor(A, B)?
END
                )))
