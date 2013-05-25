#;
(exn:pred (lambda (e) (regexp-match? "duplicate type variable or index" e)))
#lang typed/racket

;; don't allow duplicate names in indexes and tvars

(: f (All (A A ...) (A A ... A -> (List A ... A))))

(define (f a . xs)
  (map (λ: ([x : A]) a) xs))
