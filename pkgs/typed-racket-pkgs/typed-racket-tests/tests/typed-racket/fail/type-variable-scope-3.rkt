#;
(exn:pred (lambda (e) (regexp-match? "Mutation only allowed" e)))
#lang typed/racket

;; Test type variable scope

;; The 'a' is bound in two different scopes
(plambda: (a) ([x : a])
  (plambda: (a) ([y : a]) (set! x y)))
