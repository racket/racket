#;
(exn-pred #rx"expected: 0")

#lang racket

;; The purpose of this test is to make sure that contract
;; errors for (Value x) types print in a good way

(module a typed/racket
  (define: (f [x : Zero]) : Zero x)
  (provide f))

(require 'a)
(f "foo")
