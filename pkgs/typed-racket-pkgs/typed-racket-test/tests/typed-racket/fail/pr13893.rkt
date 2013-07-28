#;
(exn:pred #rx"Bad arguments to function in apply")
#lang typed/racket

;; Make sure that case-> types with multiple branches that
;; includes a * domain produce a type error instead of
;; accidentally type-checking.

;; from the PR
(: x (Listof Number))
(define x (apply + (list 1 2 "3")))

(: g (-> (Listof Number)))
(define (g) (apply + (list 1 2 "3")))

;; additional case
(: f (case-> (Integer * -> Integer)
             (Real * -> Real)))
(define (f . args) (+ 1 (list-ref args 2)))
(apply f (list 1 2 "3"))

