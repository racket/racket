#;
(exn-pred #rx"Type Checker: Declaration for `n' provided, but `n' has no definition")
#lang typed/racket

;; Test for the error message for unbound module bindings
;; with type annotations
(: n String)

