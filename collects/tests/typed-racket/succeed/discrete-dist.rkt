#lang typed/racket

;; A test case boiled down from code in the math collection

(require racket/promise)

(: sequence->normalized-weighted-samples
   (All (A) (Symbol (Sequenceof A) -> (Values (Listof A) (Listof Positive-Flonum)))))
(define (sequence->normalized-weighted-samples name xs)
  ;; the implementation of this is not important for this test
  (values (sequence->list xs) '(1.0)))

(: discrete-dist
   ;; the case-> type here causes the problem
   (All (A) (case-> ((Sequenceof A) -> (Sequenceof A))
                    ((Sequenceof A) (Option (Sequenceof Real)) -> (Sequenceof A)))))
(define (discrete-dist xs [ws #f])
  (let-values ([(xs _) (sequence->normalized-weighted-samples 'discrete-dist xs)])
    ;; the bug this test is supposed to catch causes the
    ;; following annotation to fail due to two type variables
    ;; not being equal
    (ann xs (Listof A))))
