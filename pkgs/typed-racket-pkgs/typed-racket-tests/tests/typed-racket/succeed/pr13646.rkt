#lang typed/racket

(struct: (A) Queue ([elem : A]))

(: qmap : 
   (All (A B ...) 
        (case->
          ((A -> A) (Queue A) -> (Queue A))
          ((A B ... B -> A) (Queue A) (Queue B) ... B -> (Queue A)))))
(define qmap
  (pcase-lambda:
    (A B ...)
    [([func : (A -> A)] [deq  : (Queue A)])
     deq]
    [([func : (A B ... B -> A)]
      [deq  : (Queue A)] . [deqs : (Queue B) ... B])
     deq]))
