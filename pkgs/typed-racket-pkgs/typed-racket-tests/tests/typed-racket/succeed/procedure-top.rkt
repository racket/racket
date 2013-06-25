#lang racket

;; succeed version

(module example typed/racket
  ;; coerces into unapplicable function
  (: id (Procedure -> Procedure))
  (define (id x) x)

  (: f (Integer -> Integer))
  (define (f x) (+ x 1))

  (define g (id f))

  ;; contract here should allow application
  (provide id))

(require 'example)

;; id's argument is procedure?, result is (case->)
(id +)
