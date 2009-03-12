#;
(exn-pred exn:fail:syntax? #rx".*typed module.*")
#lang scheme/load

(module m typed-scheme
  (define-struct: q ())
  (provide (all-defined-out)))

(module n scheme
  (require 'm)
  q)
