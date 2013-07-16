#lang racket
(require rackunit
         datalog/parse
         datalog/runtime
         "util.rkt")

(provide runtime-tests)

(define pc (parse-clause (open-input-string "parent(john, douglas)")))
(define pl (parse-literal (open-input-string "parent(john, douglas)")))

(define runtime-tests
  (test-suite
   "runtime"
   
   (test-suite
    "safe-clause?"
    (test-not-false "safe" (safe-clause? pc))
    (test-not-false "safe" (safe-clause? (parse-clause (open-input-string "ancestor(A, B) :- parent(A, B)"))))
    (test-false "not safe" (safe-clause? (parse-clause (open-input-string "ancestor(A, B) :- parent(jay, B)"))))
    (test-not-false "safe" (safe-clause? (parse-clause (open-input-string "ancestor(A, B) :- parent(A, C), ancestor(C, B)")))))
   
   (test-suite
    "mut simple"
    (test-equal? "empty" (prove (make-theory) pl) empty)
    (test-literal "ass->prov" 
                  (let ([thy (make-theory)])
                    (assume! thy pc)
                    (first (prove thy pl)))
                  pl)
    (test-equal? "ass->ret->prov" 
                 (let ([thy (make-theory)])
                   (assume! thy pc)
                   (retract! thy pc)
                   (prove thy pl))
                 empty)
    (test-equal? "ret->prov" 
                 (let ([thy (make-theory)])
                   (retract! thy pc)
                   (prove thy pl))
                 empty))
   
   ))

