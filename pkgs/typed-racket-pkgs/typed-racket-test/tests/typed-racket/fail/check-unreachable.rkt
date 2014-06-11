#;
(exn-pred #rx"expected: Symbol.*given: String")
#lang racket/load

;; This test makes sure that the check-unreachable-code mode
;; actually works.

(require (for-syntax typed-racket/utils/tc-utils))

(begin-for-syntax
  (check-unreachable-code? #t))

(require typed/racket)

(if #t
    "foo"
    (symbol->string "foo"))
