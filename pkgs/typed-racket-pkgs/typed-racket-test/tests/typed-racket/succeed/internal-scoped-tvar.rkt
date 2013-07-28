#lang typed/racket/base

(let ()
  (: f : (All (A) (A -> A)))
  (define (f x) ((inst values A) x))
  7)