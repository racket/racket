#lang typed/racket/base
(require racket/list)

(: f1 (All (A) (Listof A) -> (Listof A)))
(define (f1 a)
  (map (λ: ([a : A]) a) empty))

(: f2 (All (A) (Listof A) -> (Listof A)))
(define (f2 a)
  (map (λ: ([a : A]) a) empty))

