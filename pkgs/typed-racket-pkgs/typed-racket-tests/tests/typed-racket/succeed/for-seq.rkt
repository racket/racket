#lang typed/racket

(: Approximate (Natural -> Void))
(define (Approximate n) ; works
 (for: : Void ([i : Integer (in-range 10)])
     (display i)))

(for: : Void ((i : Integer (ann '(1 2 3) (Sequenceof Integer))) ; doesn't
              (j : Char "abc"))
     (display (list i j)))


(for: : Void ; doesn't
     ([from-to : (List Symbol Symbol)
               (ann '([a t] [c g]) (Sequenceof (List Symbol Symbol)))])
     #t)


(for/list: : (Listof Integer) ([i : Integer (in-range 10)]) i) ; works
