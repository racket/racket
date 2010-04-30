#;
(exn-pred exn:fail:syntax? #rx".*typed module.*")
#lang scheme/load

(module m typed-scheme
  (require (for-syntax scheme/base))
  (define-syntax (q stx) #'#f)
  (provide (all-defined-out)))

(module n scheme
  (require 'm)
  q)
