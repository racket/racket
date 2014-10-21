#;(exn-pred #rx"could not be converted to a predicate")
#lang typed/racket

;; Make sure cycles of length greater than two will
;; work in recursive type aliases too (for contract
;; kind computation)

(define-type A (Pair String B))
(define-type B (U String (Pair String C)))
(define-type C (Vector String A))

((make-predicate A) '("foo" . "bar"))
