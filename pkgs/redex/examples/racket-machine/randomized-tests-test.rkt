#lang racket

(require redex/reduction-semantics "randomized-tests.rkt")

(let ([r (run (term (application (lam (val) () (loc-noclr 0)) 'x)) (term ()) 100)])
  (test-equal
   (and (answer? r)
        (equal? (answer-value r) ''x))
   #t))

(test-predicate
 cutoff?
 (run (term (let-void 1 (let-rec ((lam () (0) (application (loc-noclr 0)))) (application (loc-noclr 0))))) (term ()) 100))

(test-predicate
 non-conf?
 (run
  (term
   (let-one 
    (lam (val) () (loc-noclr 0))
    (application
     (loc-noclr 1)
     (install-value 1 'non-proc 'x))))
  (term ())
  100))

(test-predicate
 stuck?
 (run
  (term
   (let-one 
    'x
    (let-void 
     1
     (let-rec
      ((lam () (0 1) 
            (branch (loc-noclr 1)
                    (boxenv 1 (application (loc-noclr 0)))
                    'y)))
      (application (loc-noclr 0))))))
  (term ())
  100))

(test-results)
