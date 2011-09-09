#lang typed-scheme

(require scheme/bool scheme/list typed/test-engine/scheme-tests)

(define-type-alias Atom (U Number #f))

(: mrg ([Listof Atom] [Listof Atom] -> [Listof Number]))
;; add corresponding numbers, drop false, stop at end of shortest list

(check-expect (mrg (list 1 false 2) (list 3 4 5 false 10)) (list 4 4 7))

(define (mrg l k)
 (cond
   [(or (empty? l) (empty? k))
    empty]
   [(and (not (car l)) (not (car k)))
    (cons 0 (mrg (rest l) (rest k)))]
   [(not (car l))
    (cons (car k) (mrg (rest l) (rest k)))]
   [(not (car k))
    (cons (car l) (mrg (rest l) (rest k)))]
   [else
    (cons (+ (car l) (car k)) (mrg (rest l) (rest k)))]))

(test)
