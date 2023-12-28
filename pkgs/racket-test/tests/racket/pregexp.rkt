#lang racket/base

(require rackunit)

(check-equal?
 (regexp-quote "ab-cd")
 "ab\\-cd")

(let ([re (pregexp (pregexp-quote "-"))])
  (check-true (regexp-match-exact? re "-"))
  (check-false (regexp-match-exact? re "a")))

(let ([re (pregexp (format "[~a]" (pregexp-quote ")-;")))])
  (for ([s (in-list '(")" "-" ";"))])
    (check-true (regexp-match-exact? re s)))
  ;; '8' is situated between ')' and ';' on the ASCII table, but it
  ;; _shouldn't_ match the pregexp if '-' was escaped correctly.
  (check-false (regexp-match-exact? re "8")))
