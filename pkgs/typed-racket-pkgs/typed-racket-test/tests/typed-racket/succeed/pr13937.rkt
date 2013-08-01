#lang typed/racket

;; Test for PR 13937

;; This does not directly test the issue in PR 13937, but
;; tests a workaround using `in-hash-pairs`.
;;
;; This should be a unit test, but the typecheck tests do
;; not play nice with base-special-env bindings like `in-hash-pairs`
;;
(for/hash: : (HashTable Symbol Integer)
  ([p : (Pairof Symbol Integer) (in-hash-pairs #hash((a . 5)))])
  (values (car p) (cdr p)))

