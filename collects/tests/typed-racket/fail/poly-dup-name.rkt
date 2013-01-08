#;
(exn:pred (lambda (e) (regexp-match? "duplicate type variable" e)))
#lang typed/racket

;; don't allow duplicate type variable names

(: f (All (A A) (A -> (List A))))
(define (f a) (list a))
