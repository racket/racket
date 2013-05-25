#;
(exn-pred ".*contract.*")
#lang scheme/load

(module m typed-scheme
  (: x (Number -> Number))
  (define (x n) (add1 n))
  (provide x))

(module n typed-scheme
  (require (only-in 'm))
  (require/typed 'm [x (String -> Number)])
  (x "foo"))

(require 'n)
