#lang racket/base
(require (for-syntax racket/base syntax/parse))

;; Microbenchmark for expr/c with non-liftable *result* contract.
;; See perf-nolift.rkt. The corresponding dependent contract is
;;
;;   (->i ([c contract?] [v any/c]) [_ (c) c])

(struct point (x y))
(define origin (point 0 0))

(define-syntax foo
  (syntax-parser
    [(_ (~var e (expr/c #'point? #:arg? #f)))
     #'e.c]))

(time
 (for ([i (in-range #e1e6)])
   (foo origin)))
