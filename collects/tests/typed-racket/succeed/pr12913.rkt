#lang racket

;; Test that `define-type` works at the top-level

(define ns (make-base-namespace))
(eval '(require typed/racket) ns)
(eval '(define-type Foo (U String Symbol)) ns)
(eval '(: x Foo) ns)
(eval '(define x 'x) ns)

