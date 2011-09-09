
#lang typed-scheme
(require scheme/list)

(define-type-alias (ListOf X) (U Empty (Cons X)))
(define-struct: Empty ())
(define-struct: (X) Cons ((first : X) (rest : (ListOf X))))

(: sum ((ListOf Number) -> Number))
(define (sum alon)
 (cond
   [(Empty? alon) 0]
   [else (+ (Cons-first alon)
            (sum (Cons-rest alon)))]))


(: sum2 ((ListOf Number) -> Number))
(define (sum2 alon)
 (cond
   [(Empty? alon) 0]
   [(Cons? alon) (+ (Cons-first alon)
                    (sum2 (Cons-rest alon)))]))

(sum (make-Cons 5 (make-Cons 3 (make-Cons 1 (make-Empty)))))
(sum2 (make-Cons 5 (make-Cons 3 (make-Cons 1 (make-Empty)))))
