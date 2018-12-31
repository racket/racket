#lang racket/base
(require (for-syntax racket/base syntax/parse))

;; Microbenchmark for expr/c with non-liftable contract.

;; The contract expression cannot be lifted because point? is defined
;; in the same module, and expr/c cannot be sure that the contract
;; would be lifted to a point before the definition.

;; On the other hand, this should be no worse than a dependent contract like
;;
;;   (->i ([c contract?] [v (c) c]) any)

(struct point (x y))
(define origin (point 0 0))

(define-syntax foo
  (syntax-parser
    [(_ (~var e (expr/c #'point?)))
     #'e.c]))

(time
 (for ([i (in-range #e1e6)])
   (foo origin)))
