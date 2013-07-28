#lang typed/racket/base


(: f1 (All (A) (A -> A)))
(define f1 (lambda: ((x : A)) x))

(: f2 (All (A) (A A A -> A)))
(define f2
  (ann 
    (plambda: (C) ((x : A) (y : B) (z : C)) (or x y z))
    (All (B) (B B B -> B))))

(: f3 (All (A ...) (All (B ...) (A ... A -> (B ... B -> Natural)))))
(define f3 (lambda: (x : A ... A) (lambda: (y : B ... B) (+ (length x) (length y)))))

;; PR 13622
(: f4 (All (x) (All (y z) (x x x -> Any))))
(define f4 (plambda: (x) ((x : x) (y : x) (z : x)) (or x y z)))

;; PR 13539
(: f5 (All (A) (All (B) (A B -> Integer))))
(define (f5 x y)
  (: z B)
  (define z y)
  5)
