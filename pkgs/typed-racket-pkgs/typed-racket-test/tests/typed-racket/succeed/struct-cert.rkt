#lang scheme/load

(module for-broken typed-scheme

  (define-typed-struct type ())

  (provide (all-defined-out)))

(module broken typed-scheme

  (require (prefix-in t: 'for-broken))
  (define-typed-struct binding  ([type : t:type]))
  ;; Comment out the below and it works fine.
  (provide (all-defined-out)))

(require 'broken)
