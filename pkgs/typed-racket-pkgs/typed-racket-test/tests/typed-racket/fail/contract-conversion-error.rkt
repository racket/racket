#;
(exn-pred #rx"function type has two cases of arity 1")
#lang racket/load

;; This tests the error message for contract generation errors
;; that come from module provides.
;;
;; In particular, it should give a reason for why it failed

(module a typed/racket (define v values) (provide v))
(require 'a)
v
