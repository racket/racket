#lang racket/base

;; Microbenchmark for expr/c with liftable contract.

(module helper racket/base
  (require (for-syntax racket/base syntax/parse))
  (struct point (x y))
  (define origin (point 0 0))
  (define-syntax foo
    (syntax-parser
      [(_ (~var e (expr/c #'point?)))
       #'e.c]))
  (provide foo origin))
(require 'helper)

(time
 (for ([i (in-range #e1e6)])
   (foo origin)))
