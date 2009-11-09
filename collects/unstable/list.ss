#lang scheme

; list-prefix : list? list? -> (or/c list? false/c)
; Is l a prefix or r?, and what is that prefix?
(define (list-prefix? ls rs)
  (match ls
    [(list)
     #t]
    [(list-rest l0 ls)
     (match rs
       [(list)
        #f]
       [(list-rest r0 rs)
        (if (equal? l0 r0)
            (list-prefix? ls rs)
            #f)])]))

(provide/contract
 [list-prefix? (list? list? . -> . boolean?)])
