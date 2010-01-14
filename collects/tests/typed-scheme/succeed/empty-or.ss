#lang typed/scheme

(require scheme/list)
(define-type-alias Atom (U Number #f))

(: mrg ([Listof Atom] [Listof Atom] -> [Listof Number]))
;; add corresponding numbers, drop false, stop at end of shortest list

;(check-expect (mrg (list 1 false 2) (list 3 4 5 false 10)) (list 4 4 7))

(define (mrg l k)
 (cond
   [(if (empty? l) #t (empty? k))
    empty]
   [(and (number? (car l)) (number? (car k)))
    (cons (+ (car l) (car k)) (mrg (cdr l) (cdr k)))]
   [(number? (car l))
    (cons (car l) (mrg (rest l) (rest k)))]
   [else
    (error 'fail)]))

;(test)